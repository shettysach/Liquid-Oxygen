module Environment where

import AST                 (Env (Env), Literal)
import Control.Applicative ((<|>))
import Data.Map            as Map

-- data Env = Env (Map String Literal) (Maybe Env)

global :: Env
global = Env Map.empty Nothing

local :: Env -> Env
local env = Env Map.empty (Just env)

define :: String -> Literal -> Env -> Env
define name value (Env scope prev) = Env (Map.insert name value scope) prev

get :: String -> Env -> Maybe Literal
get name (Env scope prev) = Map.lookup name scope <|> (prev >>= get name)
