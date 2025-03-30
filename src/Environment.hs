module Environment where

import Data.Map as Map

import Syntax   (Env (Env), Literal, Name)

-- data Env = Env (Map String Literal) (Maybe Env)

global :: Env
global = Env Map.empty Nothing

get :: String -> Env -> Maybe Literal
get name (Env scope _) = Map.lookup name scope

initialize :: String -> Literal -> Env -> Env
initialize name value (Env scope prev) = Env (Map.insert name value scope) prev

assign :: String -> Literal -> Int -> Env -> Maybe Env
assign name value 0 (Env scope prev) =
  if Map.member name scope
    then Just (Env (Map.insert name value scope) prev)
    else Nothing
assign name value dist (Env scope (Just prev)) =
  Env scope . Just <$> assign name value (dist - 1) prev
assign _ _ _ _ = Nothing

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

progenitor :: Env -> Env
progenitor env@(Env _ prev) = case prev of
  Nothing      -> env
  (Just prev') -> progenitor prev'

-- Scope

type Scope = Map.Map String Bool

begin :: [Scope] -> [Scope]
begin stack = Map.empty : stack

declare :: String -> [Scope] -> [Scope]
declare name stack = case stack of
  scope : scopes -> Map.insert name False scope : scopes
  []             -> stack

define :: String -> [Scope] -> [Scope]
define name stack = case stack of
  scope : scopes -> Map.insert name True scope : scopes
  []             -> stack

-- Distances

type Distances = Map Name Int

distance :: Name -> Distances -> Maybe Int
distance = Map.lookup

resolveEnv :: Name -> Distances -> Env -> Env
resolveEnv name dists env =
  case distance name dists of
    Just dist -> ancestor dist env
    Nothing   -> progenitor env
