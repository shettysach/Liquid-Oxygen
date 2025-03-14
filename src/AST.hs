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
  | Call Expr [Expr]
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

type Positioned a = (a, (Int, Int))

type Variable = Positioned String
type UnaryOp = Positioned UnaryOp'
type BinaryOp = Positioned BinaryOp'
type LogicalOp = Positioned LogicalOp'

-- Bool

isTruthy :: Literal -> Bool
isTruthy (Bool' b) = b
isTruthy Nil       = False
isTruthy _         = True

-- Show

instance Show Literal where
  show (String' s)   = s
  show (Number' n)   = show n
  show (Bool' True)  = "true"
  show (Bool' False) = "false"
  show Nil           = "nil"
