{-# LANGUAGE LambdaCase #-}

module Interpreter where

import Ast
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
      Right (lit, env') -> interpret' stmts (defVar name lit env')
    Print expr -> case evaluate expr env of
      Left err          -> return (Left err)
      Right (lit, env') -> print lit >> interpret' stmts env'
    Block stmts' ->
      interpret' stmts' (local env) >>= \case
        Left err -> return (Left err)
        Right _ -> interpret' stmts env

evaluate :: Expr -> Env -> Either RuntimeError (Literal, Env)
evaluate (Literal lit) env = Right (lit, env)
evaluate (Grouping expr) env = evaluate expr env
evaluate (Variable name) env =
  case getVar name env of
    Just value -> Right (value, env)
    Nothing    -> Left $ RuntimeError $ "Undefined var: " ++ name
evaluate (Assignment name expr) env =
  case getVar name env of
    Just _ -> do
      (lit, env') <- evaluate expr env
      Right (lit, defVar name lit env')
    Nothing -> Left $ RuntimeError $ "Undefined var: " ++ name
evaluate (Unary Minus' right) env =
  case evaluate right env of
    Right (Number' n, env') -> Right (Number' (-n), env')
    Right _                 -> Left $ RuntimeError "Invalid operand"
    Left err                -> Left err
evaluate (Unary Bang right) env =
  case evaluate right env of
    Right (lit, env') -> Right (Bool' . not . isTruthy $ lit, env')
    Left err          -> Left err
evaluate (Binary op left right) env = case evalBinary op left right env of
  Right lit -> Right (lit, env)
  Left err  -> Left err

evalBinary :: BinaryOp -> Expr -> Expr -> Env -> Either RuntimeError Literal
evalBinary op left right env = do
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
    (Bool' l, Bool' r) -> case op of
      Or  -> Right $ Bool' $ l || r
      And -> Right $ Bool' $ l && r
      _   -> invalidOp
    (_, _) -> case op of
      EqualEqual -> Right $ Bool' $ left' == right'
      BangEqual  -> Right $ Bool' $ left' /= right'
      _          -> invalidOp
 where
  invalidOp =
    Left $ RuntimeError "Invalid operands"

isTruthy :: Literal -> Bool
isTruthy Nil       = False
isTruthy (Bool' b) = b
isTruthy _         = True
