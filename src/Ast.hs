module Ast where

data Stmt
  = Expr Expr
  | Print Expr
  | Var String Expr
  deriving (Show)

data Expr
  = Literal Literal
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Grouping Expr
  | Variable String
  | Assignment String Expr
  deriving (Show)

data Literal = Number' Double | String' String | Bool' Bool | Nil
  deriving (Eq)

data UnaryOp = Minus' | Bang
  deriving (Show)

data BinaryOp
  = Slash
  | Star
  | Plus
  | Minus
  | And
  | Or
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | EqualEqual
  | BangEqual
  deriving (Show)

instance Show Literal where
  show (Bool' b)   = show b
  show (Number' n) = show n
  show (String' s) = s
  show Nil         = "nil"
