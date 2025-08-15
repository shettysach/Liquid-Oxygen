module Syntax where

import Data.HashMap.Strict (HashMap)
import Data.IORef          (IORef)

import Data.Word           (Word8)
import Error               (RuntimeError)
import Position            (Position)

data Stmt
  = Expr Expr
  | Var String' (Maybe Expr)
  | Print Expr
  | Block [Stmt]
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  | Function FnStmt
  | Return (Maybe' Expr)
  | Class String' (Maybe Expr) [FnStmt]

data FnStmt = FnStmt String' [String'] [Stmt]

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
  | Super Position String'
  | This Position

data Literal
  = Number' Double
  | String' String
  | Bool' Bool
  | Function' FnLit
  | Class' ClsLit
  | Instance' ClsLit (IORef (HashMap String Literal))
  | NativeFn String Callable Word8
  | Nil

data Env = Env (IORef (HashMap String Literal)) (Maybe Env)

type Callable = [Literal] -> Env -> IO (Either RuntimeError (Literal, Env))

data FnLit = FnLit String' Callable Word8 Env

data ClsLit = ClsLit String' (Maybe ClsLit) (HashMap String FnLit)

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

type Positioned a = (a, Position)

type Expr' = Positioned Expr
type String' = Positioned String
type Number' = Positioned Double
type Bool' = Positioned Bool
type Maybe' a = Positioned (Maybe a)
type UnaryOp' = Positioned UnaryOp
type BinaryOp' = Positioned BinaryOp
type LogicalOp' = Positioned LogicalOp

-- Traits

isTruthy :: Literal -> Bool
isTruthy (Bool' b) = b
isTruthy Nil       = False
isTruthy _         = True

instance Eq Literal where
  String' l == String' r     = l == r
  Number' l == Number' r     = l == r
  Bool' l == Bool' r         = l == r
  Nil == Nil                 = True
  Function' l == Function' r = l == r
  _ == _                     = False

instance Eq FnLit where
  FnLit n _ a _ == FnLit n' _ a' _ = n == n' && a == a'

instance Show Literal where
  show (String' s)                  = s
  show (Number' n)                  = show n
  show (Bool' True)                 = "true"
  show (Bool' False)                = "false"
  show (Function' (FnLit f _ _ _))  = "<fn " ++ fst f ++ ">"
  show (Class' (ClsLit c _ _))      = "<class " ++ fst c ++ ">"
  show (Instance' (ClsLit c _ _) _) = "<instance " ++ fst c ++ ">"
  show NativeFn{}                   = "<native fn>"
  show Nil                          = "nil"

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
