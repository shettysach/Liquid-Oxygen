module Environment where

import Ast
import Control.Applicative ((<|>))
import Data.Map            as Map

data Env = Env (Map String Literal) (Maybe Env)

global :: Env
global = Env Map.empty Nothing

local :: Env -> Env
local env = Env Map.empty (Just env)

defVar :: String -> Literal -> Env -> Env
defVar name value (Env scope prev) = Env (Map.insert name value scope) prev

getVar :: String -> Env -> Maybe Literal
getVar name (Env scope prev) = Map.lookup name scope <|> (prev >>= getVar name)
