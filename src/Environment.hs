module Environment where

import Data.Map     as Map

import Data.Functor ((<&>))
import Error        (ResolveError (ResolveError), RuntimeError (RuntimeError))
import Syntax       (Env (Env), Literal, String')

-- data Env = Env (Map String Literal) (Maybe Env)

global :: Env
global = Env Map.empty Nothing

get :: String' -> Env -> Either RuntimeError Literal
get (var, pos) (Env scope _) = case Map.lookup var scope of
  Just lit -> Right lit
  Nothing  -> Left $ RuntimeError "Undefined var" var pos

initialize :: String -> Literal -> Env -> Env
initialize name value (Env scope prev) = Env (Map.insert name value scope) prev

assign :: String' -> Literal -> Int -> Env -> Either RuntimeError Env
assign (var, _) value 0 (Env scope prev)
  | Map.member var scope =
      Right $ Env (Map.insert var value scope) prev
assign name value dist (Env scope (Just prev)) =
  Env scope . Just <$> assign name value (dist - 1) prev
assign (var, pos) _ _ _ = Left $ RuntimeError "Undefined var" var pos

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

declare :: String' -> [Scope] -> Either ResolveError [Scope]
declare (var, pos) stack = case stack of
  scope : scopes
    | not (Map.member var scope) ->
        Right $ Map.insert var False scope : scopes
    | otherwise -> Left $ ResolveError "Var already defined" var pos
  [] -> Right stack

define :: String' -> [Scope] -> [Scope]
define name stack = case stack of
  scope : scopes -> Map.insert (fst name) True scope : scopes
  []             -> stack

declareDefine :: String' -> [Scope] -> Either ResolveError [Scope]
declareDefine name stack = declare name stack <&> define name

-- Distances

type Distances = Map String' Int

calcDistance :: Int -> String' -> [Scope] -> Maybe Int
calcDistance dist name' stack' = case stack' of
  scope : scopes
    | Map.member (fst name') scope -> Just dist
    | otherwise -> calcDistance (dist + 1) name' scopes
  [] -> Nothing

getDistance :: String' -> Distances -> Either RuntimeError Int
getDistance name dists = case Map.lookup name dists of
  Just dist -> Right dist
  Nothing   -> Left $ RuntimeError "Undefined var" `uncurry` name

resolveEnv :: String' -> Distances -> Env -> Env
resolveEnv name dists env = case getDistance name dists of
  Right dist -> ancestor dist env
  Left _     -> progenitor env
