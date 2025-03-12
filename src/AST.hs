module AST where

data Stmt
  = Expr Expr
  | Var String Expr
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  | Print Expr
  | Block [Stmt]
  deriving (Show)

data Expr
  = Literal Literal
  | Variable Variable
  | Assignment Variable Expr
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Logical LogicalOp Expr Expr
  | Grouping Expr
  deriving (Show)

data Literal = Number' Double | String' String | Bool' Bool | Nil
  deriving (Eq)

data UnaryOp' = Minus' | Bang
  deriving (Show, Eq)

data BinaryOp'
  = Slash
  | Star
  | Plus
  | Minus
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | EqualEqual
  | BangEqual
  deriving (Show, Eq)

data LogicalOp' = And | Or
  deriving (Show)

type Position a = (a, (Int, Int))
type Variable = Position String
type UnaryOp = Position UnaryOp'
type BinaryOp = Position BinaryOp'
type LogicalOp = Position LogicalOp'

-- Show

instance Show Literal where
  show (String' s)   = s
  show (Number' n)   = show n
  show (Bool' True)  = "true"
  show (Bool' False) = "false"
  show Nil           = "nil"
