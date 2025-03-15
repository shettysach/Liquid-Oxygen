module Interpreter where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)

import AST
import Environment

interpret :: [Stmt] -> IO (Either RuntimeError (Value, Env))
interpret statements = runExceptT (interpret' statements global)
 where
  interpret' :: [Stmt] -> Env -> ExceptT RuntimeError IO (Value, Env)
  interpret' [] env = return (Literal' Nil, env)
  interpret' (stmt : stmts) env = case stmt of
    Expr expr ->
      (ExceptT . return) (evaluate expr env) >>= interpret' stmts . snd
    Var name expr ->
      (ExceptT . return) (evaluate expr env) >>= interpret' stmts . uncurry (define name)
    Print expr ->
      (ExceptT . return) (evaluate expr env) >>= \(lit, env') ->
        (liftIO . print) lit >> interpret' stmts env'
    If cond thenStmt elseStmt ->
      (ExceptT . return) (evaluate cond env) >>= \(cond', env') ->
        if isTruthy cond'
          then interpret' (thenStmt : stmts) env'
          else case elseStmt of
            Just stmt' -> interpret' (stmt' : stmts) env'
            Nothing    -> interpret' stmts env'
    While cond stmt' -> while env
     where
      while env1 =
        (ExceptT . return) (evaluate cond env1) >>= \(cond', env2) ->
          if isTruthy cond'
            then case stmt' of
              Block stmts' -> interpret' stmts' (local env2) >>= while . snd
              _            -> interpret' [stmt'] (local env2) >>= while . snd
            else interpret' stmts env2
    Block stmts' ->
      interpret' stmts' (local env) >> interpret' stmts env

--

evaluate :: Expr -> Env -> Either RuntimeError (Value, Env)
evaluate (Literal lit) env = Right (Literal' lit, env)
evaluate (Grouping expr) env = evaluate expr env
evaluate (Variable var) env = case get (fst var) env of
  Just value -> Right (value, env)
  Nothing    -> Left $ uncurry (RuntimeError "Undefined var") var
evaluate (Assignment var expr) env = case get (fst var) env of
  Just _ ->
    evaluate expr env >>= \(lit, env') ->
      Right (lit, define (fst var) lit env')
  Nothing -> Left $ uncurry (RuntimeError "Undefined var") var
evaluate (Logical op left right) env =
  visitLogical op left right env
evaluate (Unary op right) env = do
  (right', env') <- evaluate right env
  (,env') <$> visitUnary op right'
evaluate (Binary op left right) env = do
  (left', env1) <- evaluate left env
  (right', env2) <- evaluate right env1
  (,env2) <$> visitBinary op left' right'
evaluate (Call callee args) env = do
  (callee', env1) <- evaluate callee env
  (args', env2) <- visitArgs args env1
  evaluate callee env

--

visitArgs :: [Expr] -> Env -> Either RuntimeError ([Value], Env)
visitArgs = visit []
 where
  visit lits [] env = Right (reverse lits, env)
  visit lits (arg : args) env =
    evaluate arg env >>= \(lit, env') ->
      visit (lit : lits) args env'

visitLogical ::
  LogicalOp -> Expr -> Expr -> Env -> Either RuntimeError (Value, Env)
visitLogical op left right env = do
  (left', env') <- evaluate left env
  case fst op of
    Or | isTruthy left'          -> Right (left', env')
    And | (not . isTruthy) left' -> Right (left', env')
    _                            -> evaluate right env

visitUnary :: UnaryOp -> Value -> Either RuntimeError Value
visitUnary op right
  | fst op == Minus' = case right of
      Literal' (Number' n) -> Right $ Literal' $ Number' (-n)
      _                    -> Left $ RuntimeError "Invalid operand" (show Minus') (snd op)
  | otherwise = (Right . Literal' . Bool' . not . isTruthy) right

visitBinary :: BinaryOp -> Value -> Value -> Either RuntimeError Value
visitBinary op left right
  | fst op == EqualEqual = do
      Right $ Literal' $ Bool' (left == right)
  | fst op == BangEqual = do
      Right $ Literal' $ Bool' (left /= right)
  | otherwise = do
      case (left, right) of
        (Literal' (Number' l), Literal' (Number' r)) -> case fst op of
          Minus        -> Right $ Literal' $ Number' $ l - r
          Slash        -> Right $ Literal' $ Number' $ l / r
          Star         -> Right $ Literal' $ Number' $ l * r
          Plus         -> Right $ Literal' $ Number' $ l + r
          Greater      -> Right $ Literal' $ Bool' $ l > r
          GreaterEqual -> Right $ Literal' $ Bool' $ l >= r
          Less         -> Right $ Literal' $ Bool' $ l < r
          LessEqual    -> Right $ Literal' $ Bool' $ l <= r
          _            -> invalidOp
        (Literal' (String' l), Literal' (String' r)) -> case fst op of
          Plus -> Right $ Literal' $ String' $ l ++ r
          _    -> invalidOp
        _ -> invalidOp
 where
  node = show . fst $ op
  invalidOp = Left $ RuntimeError "Invalid operands" node (snd op)

-- isTruthy

isTruthy :: Value -> Bool
isTruthy (Literal' (Bool' b)) = b
isTruthy (Literal' Nil)       = False
isTruthy _                    = True
