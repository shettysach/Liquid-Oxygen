module Ast where

data Stmt
  = Expr Expr
  | Print Expr
  | Var String (Maybe Expr)
  | Block [Stmt]
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  | Function String [String] [Stmt]
  | Return (Maybe Expr)
  deriving (Show)

data Expr
  = Literal Literal
  | Variable String
  | Assign String Expr
  | Unary UnaryOp Expr
  | Binary Expr BinaryOp Expr
  | Grouping Expr
  deriving (Show)

data Literal = Number' Double | String' String | Bool' Bool | Nil
  deriving (Show)

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
