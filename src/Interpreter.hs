{-# LANGUAGE LambdaCase #-}

module Interpreter where

import Control.Arrow              (first)
import Control.Monad              (foldM)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)

import AST
import Environment
import Error                      (RuntimeError (RuntimeError))

interpret :: [Stmt] -> IO (Either RuntimeError (Literal, Env))
interpret statements = runExceptT (interpret' statements global)

interpret' :: [Stmt] -> Env -> ExceptT RuntimeError IO (Literal, Env)
interpret' [] env = return (Nil, env)
interpret' (stmt : stmts) env = case stmt of
  Return expr ->
    ExceptT (evaluate expr env)
  Expr expr ->
    ExceptT (evaluate expr env) >>= interpret' stmts . snd
  Var name expr ->
    ExceptT (evaluate expr env) >>= interpret' stmts . uncurry (define name)
  Block stmts' ->
    interpret' stmts' (local env) >> interpret' stmts env
  Fun name params stmts' ->
    let callable args env' =
          let envf = foldr (uncurry define) (local env') (zip params args)
           in runExceptT (interpret' stmts' envf)
        fun = Function' callable (length params)
     in interpret' stmts (define name fun env)
  Print expr ->
    ExceptT (evaluate expr env) >>= \(lit, env') ->
      (liftIO . print) lit >> interpret' stmts env'
  If cond thenStmt elseStmt ->
    ExceptT (evaluate cond env) >>= \(cond', env') ->
      if isTruthy cond'
        then interpret' (thenStmt : stmts) env'
        else case elseStmt of
          Just stmt' -> interpret' (stmt' : stmts) env'
          Nothing    -> interpret' stmts env'
  While cond stmt' -> while env
   where
    while env' =
      ExceptT (evaluate cond env') >>= \(cond', envc) ->
        if isTruthy cond'
          then case stmt' of
            Block stmts' -> interpret' stmts' (local envc) >>= while . snd
            _            -> interpret' [stmt'] (local envc) >>= while . snd
          else interpret' stmts envc

--

evaluate :: Expr -> Env -> IO (Either RuntimeError (Literal, Env))
evaluate (Literal lit) env = return $ Right (lit, env)
evaluate (Grouping expr) env = evaluate expr env
evaluate (Variable var) env = case get (fst var) env of
  Just value -> return $ Right (value, env)
  Nothing    -> return $ Left $ uncurry (RuntimeError "Undefined var") var
evaluate (Assignment var expr) env = case get (fst var) env of
  Just _ ->
    evaluate expr env >>= \case
      Left err -> return $ Left err
      Right (lit, env') -> return $ Right (lit, define (fst var) lit env')
  Nothing -> return $ Left $ uncurry (RuntimeError "Undefined var") var
evaluate (Logical op left right) env =
  visitLogical op left right env
evaluate (Unary op right) env = runExceptT $ do
  (right', env') <- ExceptT $ evaluate right env
  result <- ExceptT $ return $ visitUnary op right'
  return (result, env')
evaluate (Binary op left right) env = runExceptT $ do
  (left', envl) <- ExceptT $ evaluate left env
  (right', envr) <- ExceptT $ evaluate right envl
  result <- ExceptT $ return $ visitBinary op left' right'
  return (result, envr)
evaluate (Call callee args) env = runExceptT $ do
  (callee', envc) <- ExceptT (evaluate callee env)
  (args', envf) <- fold args envc
  ExceptT (visitCall callee' args' envf)
 where
  visit (lits, env') arg = first (: lits) <$> ExceptT (evaluate arg env')
  fold args' env' = first reverse <$> foldM visit ([], env') args'

--

visitCall :: Literal -> [Literal] -> Env -> IO (Either RuntimeError (Literal, Env))
visitCall (Function' fun arity) args env
  | length args /= arity = return $ Left $ RuntimeError "Arity" (show arity) (0, 0)
  | otherwise = fun args env
visitCall _ _ _ = return $ Left $ RuntimeError "Calling non-function" "idk" (0, 0)

visitLogical ::
  LogicalOp -> Expr -> Expr -> Env -> IO (Either RuntimeError (Literal, Env))
visitLogical op left right env = runExceptT $ do
  (left', env') <- ExceptT $ evaluate left env
  case fst op of
    Or | isTruthy left'          -> return (left', env')
    And | (not . isTruthy) left' -> return (left', env')
    _                            -> ExceptT $ evaluate right env'

visitUnary :: UnaryOp -> Literal -> Either RuntimeError Literal
visitUnary op right
  | fst op == Minus' = case right of
      Number' n -> Right $ Number' (-n)
      _         -> Left $ RuntimeError "Invalid operand" (show Minus') (snd op)
  | otherwise = (Right . Bool' . not . isTruthy) right

visitBinary :: BinaryOp -> Literal -> Literal -> Either RuntimeError Literal
visitBinary op left right
  | fst op == EqualEqual = do
      Right $ Bool' (left == right)
  | fst op == BangEqual = do
      Right $ Bool' (left /= right)
  | otherwise = do
      case (left, right) of
        (Number' l, Number' r) -> case fst op of
          Minus        -> Right $ Number' $ l - r
          Slash        -> Right $ Number' $ l / r
          Star         -> Right $ Number' $ l * r
          Plus         -> Right $ Number' $ l + r
          Greater      -> Right $ Bool' $ l > r
          GreaterEqual -> Right $ Bool' $ l >= r
          Less         -> Right $ Bool' $ l < r
          LessEqual    -> Right $ Bool' $ l <= r
          _            -> Left invalid
        (String' l, String' r) -> case fst op of
          Plus -> Right $ String' $ l ++ r
          _    -> Left invalid
        _ -> Left invalid
 where
  invalid =
    RuntimeError
      "Invalid operands"
      (show . fst $ op)
      (snd op)
