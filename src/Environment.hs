{-# LANGUAGE ImportQualifiedPost #-}

module Environment where

import Control.Monad ((>=>))
import Data.Functor  (($>), (<&>))
import Data.IORef    (newIORef, readIORef, writeIORef)
import Data.List     qualified as List
import Data.Map      qualified as Map

import Error         (ResolveError (ResolveError), RuntimeError (RuntimeError))
import Syntax        (Env (Env), Literal, String')

global :: Env
global = Env Map.empty Nothing

initialize :: String -> Literal -> Env -> IO Env
initialize name value (Env scope prev) = do
  ref <- newIORef value
  let scope' = Map.insert name ref scope
  pure $ Env scope' prev

getHere :: String' -> Env -> IO (Either RuntimeError Literal)
getHere (var, pos) (Env scope _) =
  case Map.lookup var scope of
    Just ref -> readIORef ref <&> Right
    Nothing  -> pure . Left $ RuntimeError "Undefined variable" var pos

getAt :: String' -> Distances -> Env -> IO (Either RuntimeError Literal)
getAt name dists env = getHere name $ case getDistance name dists of
  Right dist -> ancestor env dist
  Left _     -> progenitor env

assignAt :: String' -> Literal -> Int -> Env -> IO (Either RuntimeError Env)
assignAt name value 0 env@(Env scope _) | Just ref <- Map.lookup (fst name) scope = writeIORef ref value $> Right env
assignAt name value d (Env scope (Just enc)) = assignAt name value (d - 1) enc $> pure (Env scope (Just enc))
assignAt (var, pos) _ _ _ = pure $ Left $ RuntimeError "Undefined variable" var pos

child :: Env -> Env
child env = Env Map.empty (Just env)

parent :: Env -> Env
parent (Env _ (Just enc)) = enc
parent _                  = undefined

ancestor :: Env -> Int -> Env
ancestor env 0                = env
ancestor (Env _ (Just enc)) d = ancestor enc (d - 1)
ancestor _ _                  = undefined

progenitor :: Env -> Env
progenitor (Env _ (Just enc)) = progenitor enc
progenitor env                = env

--

type Scope = Map.Map String Bool
type Distances = Map.Map String' Int

begin :: [Scope] -> [Scope]
begin = (Map.empty :)

declare :: String' -> [Scope] -> Either ResolveError [Scope]
declare (name, pos) (scope : _) | Map.member name scope = Left $ ResolveError "Variable already declared" name pos
declare (name, _) (scope : scopes) = Right $ Map.insert name False scope : scopes
declare _ [] = pure []

define :: String -> [Scope] -> [Scope]
define name (scope : scopes) = Map.insert name True scope : scopes
define _ []                  = []

declareDefine :: String' -> [Scope] -> Either ResolveError [Scope]
declareDefine name = declare name >=> pure . (define . fst) name

calcDistance :: String -> [Scope] -> Maybe Int
calcDistance = List.findIndex . Map.member

getDistance :: String' -> Distances -> Either RuntimeError Int
getDistance name dists = case Map.lookup name dists of
  Just dist -> Right dist
  Nothing   -> Left $ RuntimeError "Unresolved variable" `uncurry` name
