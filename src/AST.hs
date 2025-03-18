module AST where

import Data.Map as Map
import Error    (RuntimeError)

data Stmt
  = Expr Expr
  | Var String Expr
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  | Print Expr
  | Block [Stmt]
  | Fun String [String] [Stmt]
  | Return Expr
  deriving (Show)

data Expr
  = Literal Literal
  | Variable Name
  | Assignment Name Expr
  | Call Expr [Expr]
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Logical LogicalOp Expr Expr
  | Grouping Expr
  deriving (Show)

data Literal
  = Number' Double
  | String' String
  | Bool' Bool
  | Function' Callable Int
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

instance Eq Literal where
  (String' l) == (String' r) = l == r
  (Number' l) == (Number' r) = l == r
  (Bool' l) == (Bool' r)     = l == r
  Nil == Nil                 = True
  _ == _                     = False -- TODO: Function eq

isTruthy :: Literal -> Bool
isTruthy (Bool' b) = b
isTruthy Nil       = False
isTruthy _         = True

instance Show Literal where
  show (String' s)     = s
  show (Number' n)     = show n
  show (Bool' True)    = "true"
  show (Bool' False)   = "false"
  show Nil             = "nil"
  show (Function' _ a) = show a
