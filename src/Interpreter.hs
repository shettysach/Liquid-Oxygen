module Interpreter where

import Ast

data EvalError = EvalError
  { message :: String,
    expr :: Expr
  }
  deriving (Show)

evaluate :: Expr -> Either EvalError Literal
evaluate (Literal literal) = Right literal
evaluate (Grouping expr) = evaluate expr
evaluate (Unary Minus' right) = case evaluate right of
  Right (Number' n) -> Right (Number' (-n))
  Right literal ->
    Left $
      EvalError "Invalid operand" $
        Unary Minus' $
          Literal literal
  Left err -> Left err
evaluate (Unary Bang right) = Bool' . not . isTruthy <$> evaluate right
evaluate (Binary op left right) = evalBinary op left right

evalBinary :: BinaryOp -> Expr -> Expr -> Either EvalError Literal
evalBinary op left right = do
  left' <- evaluate left
  right' <- evaluate right
  case (op, left', right') of
    -- Arithmetic ops
    (Minus, Number' l, Number' r) -> Right $ Number' $ l - r
    (Slash, Number' l, Number' r) -> Right $ Number' $ l / r
    (Star, Number' l, Number' r) -> Right $ Number' $ l * r
    (Plus, Number' l, Number' r) -> Right $ Number' $ l + r
    -- String
    (Plus, String' l, String' r) -> Right $ String' $ l ++ r
    -- Comparison ops
    (Greater, Number' l, Number' r) -> Right $ Bool' $ l > r
    (GreaterEqual, Number' l, Number' r) -> Right $ Bool' $ l >= r
    (Less, Number' l, Number' r) -> Right $ Bool' $ l < r
    (LessEqual, Number' l, Number' r) -> Right $ Bool' $ l <= r
    (EqualEqual, l, r) -> Right $ Bool' $ l == r
    (BangEqual, l, r) -> Right $ Bool' $ l /= r
    -- Logical ops
    (Or, Bool' l, Bool' r) -> Right $ Bool' $ l || r
    (And, Bool' l, Bool' r) -> Right $ Bool' $ l && r
    -- Error
    _ ->
      Left $
        EvalError "Invalid operands" $
          Binary op (Literal right') (Literal right')

isTruthy :: Literal -> Bool
isTruthy Nil = False
isTruthy (Bool' b) = b
isTruthy _ = True
