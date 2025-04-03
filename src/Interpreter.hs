{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections       #-}

module Interpreter where

import Control.Arrow              (first, second)
import Control.Monad              (foldM, void)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT)
import Data.Functor               ((<&>))

import Data.Map                   qualified as Map
import Environment                as Env
import Error                      (RuntimeError (RuntimeError))
import Syntax

interpret :: ([Stmt], Distances) -> IO (Either RuntimeError ())
interpret (statements, distances) = void <$> runExceptT (interpret' statements distances global)

interpret' :: [Stmt] -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
interpret' [] _ env = return (Nil, env)
interpret' (stmt : stmts) dists env = case stmt of
  Expr expr -> evaluate expr dists env >>= interpret' stmts dists . snd
  Var var (Just expr) -> evaluate expr dists env >>= interpret' stmts dists . uncurry (initialize $ fst var)
  Var var Nothing -> interpret' stmts dists (initialize (fst var) Nil env)
  Return expr -> case fst expr of
    Just expr' -> evaluate expr' dists env <&> second Env.parent
    Nothing    -> evaluate (Literal Nil) dists env <&> second Env.parent
  Block stmts' -> interpret' stmts' dists (child env) >>= interpret' stmts dists . Env.parent . snd
  Print expr -> do
    (lit, env') <- evaluate expr dists env
    (liftIO . print) lit
    interpret' stmts dists env'
  If cond thenStmt elseStmt -> do
    (cond', env') <- evaluate cond dists env
    if isTruthy cond'
      then interpret' (thenStmt : stmts) dists env'
      else case elseStmt of
        Just stmt' -> interpret' (stmt' : stmts) dists env'
        Nothing    -> interpret' stmts dists env'
  While cond stmt' -> while env
   where
    while env' = do
      (cond', envC) <- evaluate cond dists env'
      if isTruthy cond'
        then interpret' [stmt'] dists envC >>= while . snd
        else interpret' stmts dists envC
  Function name params stmts' _ ->
    let callable args env' =
          let envF = foldr (uncurry initialize) (child env') (zip (map fst params) args)
           in runExceptT (interpret' stmts' dists envF)
        fun = Function' name callable (length params)
     in interpret' stmts dists (initialize (fst name) fun env)
  Class name _methods ->
    let klass = Class' name
     in interpret' stmts dists (initialize (fst name) klass env)

evaluate :: Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
evaluate (Literal lit) _ env = except $ Right (lit, env)
evaluate (Grouping expr) dists env = evaluate expr dists env
evaluate (Variable var) dists env = except $ Env.get var (resolveEnv var dists env) >>= Right . (,env)
evaluate (Assignment var expr) dists env = do
  (lit, env') <- evaluate expr dists env
  except $
    Env.getDistance var dists
      >>= Env.assign var lit `flip` env'
      >>= Right . (lit,)
evaluate (Unary op right) dists env = do
  (r, envR) <- evaluate right dists env
  except $ visitUnary op r <&> (,envR)
evaluate (Binary op left right) dists env = do
  (l, envL) <- evaluate left dists env
  (r, envR) <- evaluate right dists envL
  except $ visitBinary op l r <&> (,envR)
evaluate (Logical op left right) dists env =
  visitLogical op left right dists env
evaluate (Call callee args) dists env = do
  callee' <- fst <$> evaluate (fst callee) dists env
  (args', closure') <- foldArgs args closure
  lit <- fst <$> ExceptT (visitCall (snd callee) callee' args' closure')
  except $ Right (lit, env)
 where
  closure = case fst callee of
    Variable var -> resolveEnv var dists env
    _            -> undefined
  evalArg (lits, env') arg =
    first (: lits) <$> evaluate arg dists env'
  foldArgs args' closure' =
    first reverse <$> foldM evalArg ([], closure') args'
evaluate (Get expr prop) dists env = do
  (inst, envI) <- evaluate expr dists env
  except $ case inst of
    Instance' _ props -> case Map.lookup (fst prop) props of
      Just lit -> Right (lit, envI)
      Nothing  -> Left $ uncurry (RuntimeError "Undefined prop") prop
    _ -> Left $ uncurry (RuntimeError "Only instances have props") prop
evaluate (Set expr prop expr') dists env = do
  (inst, envI) <- evaluate expr dists env
  (lit, envL) <- evaluate expr' dists envI
  except $ case inst of
    Instance' name props -> do
      let props' = Map.insert (fst prop) lit props
      let inst' = Instance' name props'
      case expr of
        Variable var ->
          Env.getDistance var dists
            >>= Env.assign var inst' `flip` envL
            >>= Right . (inst',)
        _ -> Right (inst', envL)
    _ -> Left $ uncurry (RuntimeError "Only instances have props") prop

visitCall :: (Int, Int) -> Literal -> Callable
visitCall _ (Function' name fun arity) args env
  | length args == arity = fun args env
  | otherwise = return $ Left $ uncurry (RuntimeError ("Arity = " ++ show arity)) name
visitCall _ (Class' name) args env
  | null args = return $ Right (Instance' name Map.empty, env)
  | otherwise = return $ Left $ uncurry (RuntimeError "Class constructor takes no arguments") name
visitCall pos lit _ _ = return $ Left $ RuntimeError "Calling non-function/non-class" (show lit) pos

visitLogical ::
  LogicalOp' -> Expr -> Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
visitLogical op left right dists env = do
  (left', env') <- evaluate left dists env
  case fst op of
    Or | isTruthy left'          -> return (left', env')
    And | (not . isTruthy) left' -> return (left', env')
    _                            -> evaluate right dists env'

visitUnary :: UnaryOp' -> Literal -> Either RuntimeError Literal
visitUnary op right
  | Minus' <- fst op = case right of
      Number' n -> Right $ Number' (-n)
      _         -> Left $ RuntimeError "Invalid operand" (show Minus') (snd op)
  | otherwise = (Right . Bool' . not . isTruthy) right

visitBinary :: BinaryOp' -> Literal -> Literal -> Either RuntimeError Literal
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
