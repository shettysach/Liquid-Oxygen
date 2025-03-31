module Syntax where

import Data.Map (Map)
import Error    (RuntimeError)

data Stmt
  = Expr Expr
  | Var Name (Maybe Expr)
  | Print Expr
  | Block [Stmt]
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  | Fun Name [Name] [Stmt]
  | Return (Maybe Expr)
  deriving (Show)

data Expr
  = Literal Literal
  | Variable Name
  | Assignment Name Expr
  | Logical LogicalOp Expr Expr
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Call Expr [Expr]
  | Grouping Expr
  deriving (Show)

data Literal
  = Number' Double
  | String' String
  | Bool' Bool
  | Function' Name Callable Int
  | Nil

type Callable = [Literal] -> Env -> IO (Either RuntimeError (Literal, Env))

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

-- Positions for Runtime Error

type Positioned a = (a, (Int, Int))

type Name = Positioned String

type UnaryOp = Positioned UnaryOp'

type BinaryOp = Positioned BinaryOp'

type LogicalOp = Positioned LogicalOp'

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
  show (Function' f _ _) = "<fun " ++ fst f ++ ">"
