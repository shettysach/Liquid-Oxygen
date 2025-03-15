module AST where

import Control.Applicative ((<|>))
import Data.Map            as Map

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

data Literal
  = Number' Double
  | String' String
  | Bool' Bool
  | Nil
  | Function Callable Int

type Callable = [Literal] -> Env -> Either RuntimeError (Literal, Env)

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

-- Traits

instance Eq Literal where
  (String' l) == (String' r) = l == r
  (Number' l) == (Number' r) = l == r
  (Bool' l) == (Bool' r)     = l == r
  _ == _                     = False -- TODO: Function eq

isTruthy :: Literal -> Bool
isTruthy (Bool' b) = b
isTruthy Nil       = False
isTruthy _         = True

instance Show Literal where
  show (String' s)    = s
  show (Number' n)    = show n
  show (Bool' True)   = "true"
  show (Bool' False)  = "false"
  show Nil            = "nil"
  show (Function _ a) = show a

-- Env

data Env = Env (Map String Literal) (Maybe Env)

global :: Env
global = Env Map.empty Nothing

local :: Env -> Env
local env = Env Map.empty (Just env)

define :: String -> Literal -> Env -> Env
define name value (Env scope prev) = Env (Map.insert name value scope) prev

get :: String -> Env -> Maybe Literal
get name (Env scope prev) = Map.lookup name scope <|> (prev >>= get name)

-- Error

data RuntimeError = RuntimeError String String (Int, Int)

instance Show RuntimeError where
  show (RuntimeError message node position) =
    "\n\ESC[31m"
      ++ "Runtime Error - "
      ++ "\ESC[0m"
      ++ message
      ++ "\nNode          - "
      ++ node
      ++ "\nPosition      - "
      ++ show position
