module Environment where

import Ast
import Data.Map as Map

type Env = Map String Literal

newEnv :: Env
newEnv = Map.empty

defVar :: String -> Literal -> Env -> Env
defVar = Map.insert

getVar :: String -> Env -> Maybe Literal
getVar = Map.lookup
