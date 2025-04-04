module Syntax where

import Data.Map (Map)
import Error    (RuntimeError)

data Stmt
  = Expr Expr
  | Var String' (Maybe Expr)
  | Print Expr
  | Block [Stmt]
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  | Function String' [String'] [Stmt] Kind
  | Return (Maybe' Expr)
  | Class String' [Stmt]
  deriving (Show)

data Kind = F | M
  deriving (Show)

data Expr
  = Literal Literal
  | Variable String'
  | Assignment String' Expr
  | Unary UnaryOp' Expr
  | Binary BinaryOp' Expr Expr
  | Logical LogicalOp' Expr Expr
  | Call Expr' [Expr]
  | Grouping Expr
  | Get Expr String'
  | Set Expr String' Expr
  | This Position
  deriving (Show)

data Literal
  = Number' Double
  | String' String
  | Bool' Bool
  | Function' String' Callable Int
  | Class' String' (Map String Literal)
  | Instance' String' (Map String Literal)
  | Nil

type Callable = [Literal] -> Env -> IO (Either RuntimeError (Literal, Env))

data UnaryOp = Minus' | Bang

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

data LogicalOp = And | Or

-- Positions for Runtime Error

type Position = (Int, Int)
type Positioned a = (a, Position)

type Expr' = Positioned Expr
type String' = Positioned String
type Number' = Positioned Double
type Bool' = Positioned Bool
type Maybe' a = Positioned (Maybe a)
type UnaryOp' = Positioned UnaryOp
type BinaryOp' = Positioned BinaryOp
type LogicalOp' = Positioned LogicalOp

-- Env

data Env = Env (Map String Literal) (Maybe Env)
  deriving (Show)

-- Traits

isTruthy :: Literal -> Bool
isTruthy (Bool' b) = b
isTruthy Nil       = False
isTruthy _         = True

instance Eq Literal where
  String' l == String' r             = l == r
  Number' l == Number' r             = l == r
  Bool' l == Bool' r                 = l == r
  Nil == Nil                         = True
  Function' l _ _ == Function' r _ _ = l == r
  _ == _                             = False

instance Show Literal where
  show (String' s)       = s
  show (Number' n)       = show n
  show (Bool' True)      = "true"
  show (Bool' False)     = "false"
  show Nil               = "nil"
  show (Function' f _ _) = "<fn " ++ fst f ++ ">"
  show (Class' c _)      = "<class " ++ fst c ++ ">"
  show (Instance' i _)   = fst i ++ " instance"

instance Show UnaryOp where
  show Minus' = "-"
  show Bang   = "!"

instance Show BinaryOp where
  show Slash        = "/"
  show Star         = "*"
  show Plus         = "+"
  show Minus        = "-"
  show Greater      = ">"
  show GreaterEqual = ">="
  show Less         = "<"
  show LessEqual    = "<="
  show EqualEqual   = "=="
  show BangEqual    = "!="

instance Show LogicalOp where
  show And = "and"
  show Or  = "or"
