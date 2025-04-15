module Environment where

import Data.Map     as Map

import Data.Functor ((<&>))
import Error        (ResolveError (ResolveError), RuntimeError (RuntimeError))
import Syntax       (Env (Env), Literal, String')

-- data Env = Env (Map String Literal) (Maybe Env)

global :: Env
global = Env Map.empty Nothing

initialize :: String -> Literal -> Env -> Env
initialize name value (Env scope prev) = Env (Map.insert name value scope) prev

getScope :: String' -> Env -> Either RuntimeError Literal
getScope name (Env scope _) = case Map.lookup (fst name) scope of
  Just lit -> Right lit
  Nothing  -> Left $ RuntimeError "Undefined var" `uncurry` name

getAt :: String' -> Distances -> Env -> Either RuntimeError Literal
getAt name dists env = getDistance name dists >>= getScope name . ancestor env

assignAt :: String' -> Literal -> Int -> Env -> Either RuntimeError Env
assignAt (var, _) value 0 (Env scope prev) | Map.member var scope = Right $ Env (Map.insert var value scope) prev
assignAt name value dist (Env scope (Just prev)) = Env scope . Just <$> assignAt name value (dist - 1) prev
assignAt (var, pos) _ _ _ = Left $ RuntimeError "Undefined var" var pos

child :: Env -> Env
child env = Env Map.empty (Just env)

parent :: Env -> Env
parent (Env _ (Just prev)) = prev
parent env                 = error (show env)

ancestor :: Env -> Int -> Env
ancestor env 0 = env
ancestor env d = ancestor (parent env) (d - 1)

progenitor :: Env -> Env
progenitor (Env _ (Just prev)) = progenitor prev
progenitor env                 = env

-- Scope

type Scope = Map.Map String Bool

begin :: [Scope] -> [Scope]
begin stack = Map.empty : stack

declare :: String' -> [Scope] -> Either ResolveError [Scope]
declare (var, pos) stack = case stack of
  scope : scopes
    | not (Map.member var scope) -> Right $ Map.insert var False scope : scopes
    | otherwise -> Left $ ResolveError "Var already defined" var pos
  [] -> Right stack

define :: String -> [Scope] -> [Scope]
define name stack = case stack of
  scope : scopes -> Map.insert name True scope : scopes
  []             -> undefined

declareDefine :: String' -> [Scope] -> Either ResolveError [Scope]
declareDefine name stack = declare name stack <&> define (fst name)

-- Distances

type Distances = Map String' Int

calcDistance :: Int -> String' -> [Scope] -> Maybe Int
calcDistance dist name stack = case stack of
  scope : scopes
    | Map.member (fst name) scope -> Just dist
    | otherwise -> calcDistance (dist + 1) name scopes
  [] -> Nothing

getDistance :: String' -> Distances -> Either RuntimeError Int
getDistance name dists = case Map.lookup name dists of
  Just dist -> Right dist
  Nothing   -> Left $ RuntimeError "Undefined var" `uncurry` name
