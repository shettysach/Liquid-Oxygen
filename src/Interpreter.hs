module Interpreter where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)

import AST
import Environment
import Error                      (RuntimeError (RuntimeError))

interpret :: [Stmt] -> IO (Either RuntimeError (Literal, Env))
interpret statements = runExceptT (interpret' statements global)
 where
  interpret' [] env = return (Nil, env)
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
    Fun name params stmts' -> do
      let callable args env1 =
            let env2 = foldr (uncurry define) (local env1) (zip params args)
             in runExceptT $ interpret' stmts' env2
      let fun = Function' callable (length params)
      interpret' stmts (define name fun env)

--

evaluate :: Expr -> Env -> Either RuntimeError (Literal, Env)
evaluate (Literal lit) env = Right (lit, env)
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
  visitCall callee' args' env2

--

visitLogical ::
  LogicalOp -> Expr -> Expr -> Env -> Either RuntimeError (Literal, Env)
visitLogical op left right env = do
  (left', env') <- evaluate left env
  case fst op of
    Or | isTruthy left'          -> Right (left', env')
    And | (not . isTruthy) left' -> Right (left', env')
    _                            -> evaluate right env

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

-- TODO: Use fold
visitArgs :: [Expr] -> Env -> Either RuntimeError ([Literal], Env)
visitArgs = visit []
 where
  visit lits [] env = Right (reverse lits, env)
  visit lits (arg : args) env =
    evaluate arg env >>= \(lit, env') -> visit (lit : lits) args env'

visitCall :: Literal -> [Literal] -> Env -> Either RuntimeError (Literal, Env)
visitCall (Function' fun arity) args env
  | length args /= arity = Left $ RuntimeError "Arity" (show arity) (0, 0)
  -- \| otherwise = fun args env
  | otherwise = undefined
visitCall _ _ _ = Left $ RuntimeError "Calling non-function" "idk" (0, 0)
