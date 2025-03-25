module Environment where

import AST                 (Env (Env), Expr, Literal)
import Control.Applicative ((<|>))
import Data.Map            as Map

-- data Env = Env (Map String Literal) (Maybe Env)

global :: Env
global = Env Map.empty Nothing

get :: String -> Env -> Maybe Literal
get name (Env scope prev) = Map.lookup name scope <|> (prev >>= get name)

define :: String -> Literal -> Env -> Env
define name value (Env scope prev) = Env (Map.insert name value scope) prev

assign :: String -> Literal -> Env -> Maybe Env
assign name value (Env scope prev) =
  case Map.lookup name scope of
    Just _  -> Just (Env (Map.insert name value scope) prev)
    Nothing -> prev >>= assign name value >>= Just . Env scope . Just

child :: Env -> Env
child env = Env Map.empty (Just env)

parent :: Env -> Env
parent (Env _ (Just prev)) = prev
parent env                 = env

ancestor :: Int -> Env -> Env
ancestor dist env =
  if dist == 0
    then env
    else ancestor (dist - 1) (parent env)

-- Distances

type Distances = Map String Int

local :: Distances
local = Map.empty
