module AST where

data Expression
  = LiteralExpression Literal
  | UnaryOperator Expression
  | BinaryOperator Expression Expression
  | Grouping

data Literal = NumberLit | StringLit | TrueLit | FalseLit | NilLit

data UnaryOperator = Minus' | Bang

data BinaryOperator
  = Slash
  | Star
  | Plus
  | Minus
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Equal
  | BangEqual
