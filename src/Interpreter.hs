module Interpreter where

import Ast

data EvalError = EvalError
  { message :: String
  , expr    :: Expr
  }
  deriving (Show)

interpret :: [Stmt] -> IO (Either EvalError Literal)
interpret [] = return $ Right Nil
interpret (stmt : stmts) = do
  result <- case stmt of
    Print expr -> do
      case evaluate expr of
        Left err -> return (Left err)
        Right ok ->
          print ok >> return (Right Nil)
    Expr expr -> return (evaluate expr)
  case result of
    Left err -> return (Left err)
    Right _  -> interpret stmts

evaluate :: Expr -> Either EvalError Literal
evaluate (Literal literal) = Right literal
evaluate (Grouping expr) = evaluate expr
evaluate (Unary Minus' right) = case evaluate right of
  Right (Number' n) -> Right (Number' (-n))
  Right literal ->
    Left $ EvalError "Invalid operand" $ Unary Minus' $ Literal literal
  Left err -> Left err
evaluate (Unary Bang right) = Bool' . not . isTruthy <$> evaluate right
evaluate (Binary op left right) = evalBinary op left right

evalBinary :: BinaryOp -> Expr -> Expr -> Either EvalError Literal
evalBinary op left right = do
  left' <- evaluate left
  right' <- evaluate right
  case (left', right') of
    -- Number
    (Number' l, Number' r) -> case op of
      Minus        -> Right $ Number' (l - r)
      Slash        -> Right $ Number' (l / r)
      Star         -> Right $ Number' (l * r)
      Plus         -> Right $ Number' (l + r)
      Greater      -> Right $ Bool' (l > r)
      GreaterEqual -> Right $ Bool' (l >= r)
      Less         -> Right $ Bool' (l < r)
      LessEqual    -> Right $ Bool' (l <= r)
      _            -> invalidOp
    -- String
    (String' l, String' r) -> case op of
      Plus -> Right $ String' (l ++ r)
      _    -> invalidOp
    -- Boolean
    (Bool' l, Bool' r) -> case op of
      Or  -> Right $ Bool' (l || r)
      And -> Right $ Bool' (l && r)
      _   -> invalidOp
    -- Equality
    _ -> case op of
      EqualEqual -> Right $ Bool' (left' == right')
      BangEqual  -> Right $ Bool' (left' /= right')
      _          -> invalidOp
 where
  invalidOp =
    Left $ EvalError "Invalid op" $ Binary op left right

isTruthy :: Literal -> Bool
isTruthy Nil       = False
isTruthy (Bool' b) = b
isTruthy _         = True
