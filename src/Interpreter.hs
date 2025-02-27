module Interpreter where

import Ast

-- import Token (TokenType)
--
-- data RuntimeError = ParseError
--   { message :: String,
--     token :: TokenType,
--     line :: Int
--   }
--   deriving (Show)
--
evaluate :: Expr -> Literal
evaluate (Literal literal) = literal
evaluate (Grouping expr) = evaluate expr
evaluate (Unary Minus' right)
  | Number' r <- evaluate right = Number' (-r)
  | otherwise = error "-u not num"
evaluate (Unary Bang right) =
  Bool' $ not $ isTruthy $ evaluate right
evaluate (Binary op left right) =
  case (op, evaluate left, evaluate right) of
    (Minus, Number' l, Number' r) -> Number' (l - r)
    (Slash, Number' l, Number' r) -> Number' (l / r)
    (Star, Number' l, Number' r) -> Number' (l * r)
    (Plus, Number' l, Number' r) -> Number' (l + r)
    (Plus, String' l, String' r) -> String' (l ++ r)
    (Greater, Number' l, Number' r) -> Bool' (l > r)
    (GreaterEqual, Number' l, Number' r) -> Bool' (l >= r)
    (Less, Number' l, Number' r) -> Bool' (l < r)
    (LessEqual, Number' l, Number' r) -> Bool' (l <= r)
    (EqualEqual, l, r) -> Bool' (l == r)
    (BangEqual, l, r) -> Bool' (l /= r)
    (Or, Bool' l, Bool' r) -> Bool' (l || r)
    (And, Bool' l, Bool' r) -> Bool' (l && r)
    _ -> error "Invalid binary operation"

isTruthy :: Literal -> Bool
isTruthy Nil = False
isTruthy (Bool' b) = b
isTruthy _ = True
