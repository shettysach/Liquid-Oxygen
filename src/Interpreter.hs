{-# LANGUAGE LambdaCase #-}

module Interpreter where

import AST
import Environment

-- TODO: Get location context
newtype RuntimeError = RuntimeError String
  deriving (Show)

interpret :: [Stmt] -> IO (Either RuntimeError Literal)
interpret statements = interpret' statements global
 where
  interpret' [] _ = return $ Right Nil
  interpret' (stmt : stmts) env = case stmt of
    Expr expr -> case evaluate expr env of
      Left err        -> return (Left err)
      Right (_, env') -> interpret' stmts env'
    Var name expr -> case evaluate expr env of
      Left err          -> return (Left err)
      Right (lit, env') -> interpret' stmts (define name lit env')
    Print expr -> case evaluate expr env of
      Left err          -> return (Left err)
      Right (lit, env') -> print lit >> interpret' stmts env'
    If condition thenStmt elseStmt -> case evaluate condition env of
      Left err -> return (Left err)
      Right (condValue, env') ->
        if isTruthy condValue
          then interpret' [thenStmt] env'
          else maybe (interpret' stmts env') (\s -> interpret' [s] env') elseStmt
    Block stmts' ->
      interpret' stmts' (local env) >>= \case
        Left err -> return (Left err)
        Right _ -> interpret' stmts env

evaluate :: Expr -> Env -> Either RuntimeError (Literal, Env)
evaluate (Literal lit) env = Right (lit, env)
evaluate (Grouping expr) env = evaluate expr env
evaluate (Variable name) env =
  case get name env of
    Just value -> Right (value, env)
    Nothing    -> Left $ RuntimeError $ "Undefined var: " ++ name
evaluate (Assignment name expr) env =
  case get name env of
    Just _ -> do
      (lit, env') <- evaluate expr env
      Right (lit, define name lit env')
    Nothing -> Left $ RuntimeError $ "Undefined var: " ++ name
evaluate (Logical op left right) env = evalLogical op left right env
evaluate (Unary op right) env = evalUnary op right env
evaluate (Binary op left right) env = case evalBinary op left right env of
  Right lit -> Right (lit, env)
  Left err  -> Left err

--

evalLogical ::
  LogicalOp -> Expr -> Expr -> Env -> Either RuntimeError (Literal, Env)
evalLogical op left right env = do
  (left', env') <- evaluate left env
  case op of
    Or | isTruthy left'          -> Right (left', env')
    And | (not . isTruthy) left' -> Right (left', env')
    _                            -> evaluate right env

evalUnary :: UnaryOp -> Expr -> Env -> Either RuntimeError (Literal, Env)
evalUnary Minus' right env = case evaluate right env of
  Right (Number' n, env') -> Right (Number' (-n), env')
  Right _                 -> Left $ RuntimeError "Invalid operand"
  Left err                -> Left err
evalUnary Bang right env = case evaluate right env of
  Right (lit, env') -> Right (Bool' . not . isTruthy $ lit, env')
  Left err          -> Left err

evalBinary :: BinaryOp -> Expr -> Expr -> Env -> Either RuntimeError Literal
evalBinary op left right env
  | op == EqualEqual = do
      (left', _) <- evaluate left env
      (right', _) <- evaluate right env
      Right $ Bool' (left' == right')
  | op == BangEqual = do
      (left', _) <- evaluate left env
      (right', _) <- evaluate right env
      Right $ Bool' (left' /= right')
  | otherwise = do
      (left', _) <- evaluate left env
      (right', _) <- evaluate right env
      case (left', right') of
        (Number' l, Number' r) -> case op of
          Minus        -> Right $ Number' $ l - r
          Slash        -> Right $ Number' $ l / r
          Star         -> Right $ Number' $ l * r
          Plus         -> Right $ Number' $ l + r
          Greater      -> Right $ Bool' $ l > r
          GreaterEqual -> Right $ Bool' $ l >= r
          Less         -> Right $ Bool' $ l < r
          LessEqual    -> Right $ Bool' $ l <= r
          _            -> invalidOp
        (String' l, String' r) -> case op of
          Plus -> Right $ String' $ l ++ r
          _    -> invalidOp
        _ -> invalidOp
 where
  invalidOp =
    Left $
      RuntimeError $
        "Invalid operands" ++ show op

--

isTruthy :: Literal -> Bool
isTruthy Nil       = False
isTruthy (Bool' b) = b
isTruthy _         = True
