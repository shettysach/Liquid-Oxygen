module Interpreter where

import Control.Monad              (void)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except

import AST
import Environment

-- TODO: Get location context
newtype RuntimeError = RuntimeError String
  deriving (Show)

interpret :: [Stmt] -> IO (Either RuntimeError ())
interpret statements = void <$> runExceptT (interpret' statements global)
 where
  interpret' :: [Stmt] -> Env -> ExceptT RuntimeError IO (Literal, Env)
  interpret' [] env = return (Nil, env)
  interpret' (stmt : stmts) env = case stmt of
    Expr expr ->
      (ExceptT . return) (evaluate expr env) >>= interpret' stmts . snd
    Var name expr ->
      (ExceptT . return) (evaluate expr env) >>= interpret' stmts . uncurry (define name)
    Print expr -> do
      (lit, env') <- (ExceptT . return) (evaluate expr env)
      (liftIO . print) lit
      interpret' stmts env'
    If condition thenStmt elseStmt -> do
      (cond, env') <- (ExceptT . return) (evaluate condition env)
      if isTruthy cond
        then interpret' (thenStmt : stmts) env'
        else case elseStmt of
          Just stmt' -> interpret' (stmt' : stmts) env'
          Nothing    -> interpret' stmts env'
    While condition stmt' -> loop env
     where
      loop env1 = do
        (cond, env2) <- (ExceptT . return) (evaluate condition env1)
        if isTruthy cond
          then interpret' [stmt'] env2 >>= loop . snd
          else interpret' stmts env2
    Block stmts' -> interpret' stmts' (local env) >>= interpret' stmts . snd

isTruthy :: Literal -> Bool
isTruthy (Bool' b) = b
isTruthy Nil       = False
isTruthy _         = True

evaluate :: Expr -> Env -> Either RuntimeError (Literal, Env)
evaluate (Literal lit) env = Right (lit, env)
evaluate (Grouping expr) env = evaluate expr env
evaluate (Variable name) env = case get name env of
  Just value -> Right (value, env)
  Nothing    -> Left $ RuntimeError $ "Undefined var: " ++ name
evaluate (Assignment name expr) env = case get name env of
  Just _ -> do
    (lit, env') <- evaluate expr env
    Right (lit, define name lit env')
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
evalUnary Bang right env =
  evaluate right env >>= Right . Bool' . not . isTruthy . fst

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
