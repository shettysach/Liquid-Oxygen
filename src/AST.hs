module AST where

data Stmt
  = Expr Expr
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  | Print Expr
  | Var String Expr
  | Block [Stmt]

data Expr
  = Literal Literal
  | Variable Variable
  | Assignment Variable Expr
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Logical LogicalOp Expr Expr
  | Grouping Expr

data Literal = Number' Double | String' String | Bool' Bool | Nil
  deriving (Eq)

type Variable = (String, (Int, Int))
type UnaryOp = (UnaryOp', (Int, Int))
type LogicalOp = (LogicalOp', (Int, Int))
type BinaryOp = (BinaryOp', (Int, Int))

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

-- Show

instance Show Literal where
  show (Bool' b)   = show b
  show (Number' n) = show n
  show (String' s) = s
  show Nil         = "nil"
