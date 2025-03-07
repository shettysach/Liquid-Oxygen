module AST where

data Stmt
  = Expr Expr
  | If Expr Stmt (Maybe Stmt)
  | Print Expr
  | Var String Expr
  | Block [Stmt]

data Expr
  = Literal Literal
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Logical LogicalOp Expr Expr
  | Grouping Expr
  | Variable String
  | Assignment String Expr

data Literal = Number' Double | String' String | Bool' Bool | Nil
  deriving (Eq)

data UnaryOp = Minus' | Bang
  deriving (Show)

data BinaryOp
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

data LogicalOp = And | Or

instance Show Literal where
  show (Bool' b)   = show b
  show (Number' n) = show n
  show (String' s) = s
  show Nil         = "nil"
