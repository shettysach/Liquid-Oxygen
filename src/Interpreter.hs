{-# LANGUAGE TupleSections #-}

module Interpreter where

import Control.Arrow              (first, second)
import Control.Monad              (foldM, void)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT)
import Data.Functor               ((<&>))

import Environment                as Env
import Error                      (RuntimeError (RuntimeError))
import Syntax

interpret :: ([Stmt], Distances) -> IO (Either RuntimeError ())
interpret (statements, distances) = void <$> runExceptT (interpret' statements distances global)

interpret' ::
  [Stmt] -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
interpret' [] _ env = return (Nil, env)
interpret' (stmt : stmts) dists env = case stmt of
  Expr expr -> evaluateT expr dists env >>= interpret' stmts dists . snd
  Var (name, _) (Just expr) -> evaluateT expr dists env >>= interpret' stmts dists . uncurry (initialize name)
  Var (name, _) Nothing -> interpret' stmts dists (initialize name Nil env)
  Return (Just expr) -> evaluateT expr dists env <&> second Env.parent
  Return Nothing -> evaluateT (Literal Nil) dists env <&> second Env.parent
  Block stmts' -> interpret' stmts' dists (child env) >>= interpret' stmts dists . Env.parent . snd
  Print expr -> do
    (lit, env') <- evaluateT expr dists env
    (liftIO . print) lit
    interpret' stmts dists env'
  If cond thenStmt elseStmt -> do
    (cond', env') <- evaluateT cond dists env
    if isTruthy cond'
      then interpret' (thenStmt : stmts) dists env'
      else case elseStmt of
        Just stmt' -> interpret' (stmt' : stmts) dists env'
        Nothing    -> interpret' stmts dists env'
  While cond stmt' -> while env
   where
    while env' = do
      (cond', envC) <- evaluateT cond dists env'
      if isTruthy cond'
        then interpret' [stmt'] dists envC >>= while . snd
        else interpret' stmts dists envC
  Fun name params stmts' ->
    let callable args env' =
          let envF = foldr (uncurry initialize) (child env') (zip (map fst params) args)
           in runExceptT (interpret' stmts' dists envF)
        fun = Function' name callable (length params)
     in interpret' stmts dists (initialize (fst name) fun env)

evaluateT :: Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
evaluateT expr dists env = ExceptT (evaluate expr dists env)

evaluate :: Expr -> Distances -> Env -> IO (Either RuntimeError (Literal, Env))
evaluate (Literal lit) _ env = return $ Right (lit, env)
evaluate (Grouping expr) dists env = evaluate expr dists env
evaluate (Variable var) dists env =
  return $ Env.get var (resolveEnv var dists env) >>= Right . (,env)
evaluate (Assignment var expr) dists env = runExceptT $ do
  (lit, env') <- evaluateT expr dists env
  except $ case Map.lookup var dists of
    Just dist -> Env.assign var lit dist env' >>= Right . (lit,)
    _         -> Left $ uncurry (RuntimeError "Undefined var") var
evaluate (Logical op left right) dists env =
  visitLogical op left right dists env
evaluate (Unary op right) dists env = runExceptT $ do
  (r, envR) <- evaluateT right dists env
  except $ visitUnary op r <&> (,envR)
evaluate (Binary op left right) dists env = runExceptT $ do
  (l, envL) <- evaluateT left dists env
  (r, envR) <- evaluateT right dists envL
  except $ visitBinary op l r <&> (,envR)
evaluate (Call callee args) dists env = runExceptT $ do
  let closure = case callee of
        Variable var -> resolveEnv var dists env
        _            -> undefined
      evaluateArg (lits, env') arg =
        first (: lits) <$> evaluateT arg dists env'
  callee' <- fst <$> evaluateT callee dists env
  (args', closure') <- first reverse <$> foldM evaluateArg ([], closure) args
  lit <- fst <$> ExceptT (visitCall callee' args' closure')
  except $ Right (lit, env)

visitCall :: Literal -> Callable
visitCall (Function' name fun arity) args env
  | length args == arity = fun args env
  | otherwise = return $ Left $ uncurry (RuntimeError ("Arity = " ++ show arity)) name
visitCall _ _ _ = return $ Left $ RuntimeError "Calling non-function/non-class" "?" (0, 0) -- TODO position

visitLogical ::
  LogicalOp -> Expr -> Expr -> Distances -> Env -> IO (Either RuntimeError (Literal, Env))
visitLogical op left right dists env = runExceptT $ do
  (left', env') <- ExceptT $ evaluate left dists env
  case fst op of
    Or | isTruthy left'          -> return (left', env')
    And | (not . isTruthy) left' -> return (left', env')
    _                            -> ExceptT $ evaluate right dists env'

visitUnary ::
  UnaryOp -> Literal -> Either RuntimeError Literal
visitUnary op right
  | fst op == Minus' = case right of
      Number' n -> Right $ Number' (-n)
      _         -> Left $ RuntimeError "Invalid operand" (show Minus') (snd op)
  | otherwise = (Right . Bool' . not . isTruthy) right

visitBinary ::
  BinaryOp -> Literal -> Literal -> Either RuntimeError Literal
visitBinary op left right
  | EqualEqual <- fst op = Right $ Bool' (left == right)
  | BangEqual <- fst op = Right $ Bool' (left /= right)
  | otherwise = case (left, right) of
      (Number' l, Number' r) -> case fst op of
        Minus        -> Right $ Number' $ l - r
        Slash        -> Right $ Number' $ l / r
        Star         -> Right $ Number' $ l * r
        Plus         -> Right $ Number' $ l + r
        Greater      -> Right $ Bool' $ l > r
        GreaterEqual -> Right $ Bool' $ l >= r
        Less         -> Right $ Bool' $ l < r
        LessEqual    -> Right $ Bool' $ l <= r
      (String' l, String' r) -> case fst op of
        Plus -> Right $ String' $ l ++ r
        _    -> Left invalid
      _ -> Left invalid
 where
  invalid =
    RuntimeError
      "Invalid operands"
      (show $ fst op)
      (snd op)
