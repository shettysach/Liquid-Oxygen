module Environment where

import Control.Applicative ((<|>))
import Data.Map            as Map

import AST                 (Literal)

-- Value

type Callable = [Literal] -> Env -> Either RuntimeError (Literal, Env)

data Value = Literal' Literal | Function Callable Int

instance Eq Value where
  (Literal' l) == (Literal' r) = l == r
  _ == _                       = False -- TODO: Function eq

instance Show Value where
  show (Literal' literal) = show literal
  show (Function _ arity) = show arity

-- Env

data Env = Env (Map String Value) (Maybe Env)

global :: Env
global = Env Map.empty Nothing

local :: Env -> Env
local env = Env Map.empty (Just env)

define :: String -> Value -> Env -> Env
define name value (Env scope prev) = Env (Map.insert name value scope) prev

get :: String -> Env -> Maybe Value
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
