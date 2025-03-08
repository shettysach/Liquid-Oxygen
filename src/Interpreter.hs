{-# LANGUAGE LambdaCase #-}

module Interpreter where

import AST
import Environment

-- TODO: Get location context
newtype RuntimeError = RuntimeError String
  deriving (Show)

interpret :: [Stmt] -> IO (Either RuntimeError (Literal, Env))
interpret statements = interpret' statements global
 where
  interpret' [] env = return $ Right (Nil, env)
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
      Right (cond, env') ->
        if isTruthy cond
          then interpret' (thenStmt : stmts) env'
          else case elseStmt of
            Just elseStmt' -> interpret' (elseStmt' : stmts) env'
            Nothing        -> interpret' stmts env'
    While condition stmt' -> loop env
     where
      loop env1 = case evaluate condition env1 of
        Left err -> return (Left err)
        Right (cond, env2) ->
          if isTruthy cond
            then
              interpret' [stmt'] env2 >>= \case
                Left err -> return (Left err)
                Right (_, env3) -> loop env3
            else interpret' stmts env2
    Block stmts' ->
      interpret' stmts' (local env) >>= \case
        Left err -> return (Left err)
        Right (_, env') -> interpret' stmts env'

evaluate :: Expr -> Env -> Either RuntimeError (Literal, Env)
evaluate (Literal lit) env = Right (lit, env)
evaluate (Grouping expr) env = evaluate expr env
evaluate (Variable name) env = case get name env of
  Just value -> Right (value, env)
  Nothing    -> Left $ RuntimeError $ "Undefined var: " ++ name
evaluate (Assignment name expr) env = case get name env of
  Just _ -> evaluate expr env >>= \(lit, env') -> Right (lit, define name lit env')
  Nothing -> Left $ RuntimeError $ "Undefined var: " ++ name
evaluate (Logical op left right) env = evalLogical op left right env
evaluate (Unary op right) env = (,env) <$> evalUnary op right env
evaluate (Binary op left right) env = (,env) <$> evalBinary op left right env

evalLogical ::
  LogicalOp -> Expr -> Expr -> Env -> Either RuntimeError (Literal, Env)
evalLogical op left right env = do
  (left', env') <- evaluate left env
  case op of
    Or | isTruthy left'          -> Right (left', env')
    And | (not . isTruthy) left' -> Right (left', env')
    _                            -> evaluate right env

evalUnary :: UnaryOp -> Expr -> Env -> Either RuntimeError Literal
evalUnary Minus' right env = case evaluate right env of
  Right (Number' n, _) -> Right $ Number' (-n)
  Right _              -> Left $ RuntimeError "Invalid operand"
  Left err             -> Left err
evalUnary Bang right env = case evaluate right env of
  Right (lit, _) -> Right $ Bool' . not . isTruthy $ lit
  Left err       -> Left err

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
