{-# LANGUAGE ImportQualifiedPost #-}

module Environment where

import Control.Monad ((>=>))
import Data.Functor  ((<&>))
import Data.IORef    (atomicModifyIORef', modifyIORef, newIORef, readIORef)
import Data.List     qualified as List
import Data.Map      qualified as Map

import Error         (ResolveError (ResolveError), RuntimeError (RuntimeError))
import Syntax        (Env (Env), Literal, String')

newEnv :: Maybe Env -> IO Env
newEnv enc = newIORef Map.empty <&> Env `flip` enc

global :: IO Env
global = newEnv Nothing

child :: Env -> IO Env
child env = newEnv (Just env)

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

initialize :: String -> Literal -> Env -> IO Env
initialize name value env@(Env scopeRef _) = modifyIORef scopeRef (Map.insert name value) >> pure env

getHere :: String' -> Env -> IO (Either RuntimeError Literal)
getHere (name, pos) (Env scopeRef _) = do
  scope <- readIORef scopeRef
  case Map.lookup name scope of
    Just val -> pure . Right $ val
    Nothing  -> pure . Left $ RuntimeError "Undefined variable" name pos

getAt :: String' -> Distances -> Env -> IO (Either RuntimeError Literal)
getAt name dists env = getHere name $ case getDistance name dists of
  Right dist -> ancestor env dist
  Left _     -> progenitor env

assignAt :: String' -> Literal -> Int -> Env -> IO (Either RuntimeError ())
assignAt name value 0 (Env scopeRef _) =
  let assign scope =
        if Map.member (fst name) scope
          then (Map.insert (fst name) value scope, Right ())
          else (scope, Left $ RuntimeError "Undefined variable" `uncurry` name)
   in atomicModifyIORef' scopeRef assign
assignAt name value d (Env _ (Just enc)) = assignAt name value (d - 1) enc
assignAt name _ _ _ = pure . Left $ RuntimeError "Undefined variable" `uncurry` name

--

type Scope = Map.Map String Bool
type Distances = Map.Map String' Int

begin :: [Scope] -> [Scope]
begin = (Map.empty :)

declare :: String' -> [Scope] -> Either ResolveError [Scope]
declare (name, pos) (scope : _) | Map.member name scope = Left $ ResolveError "Variable already declared" name pos
declare (name, _) (scope : scopes) = Right $ Map.insert name False scope : scopes
declare _ _ = undefined

define :: String -> [Scope] -> [Scope]
define name (scope : scopes) = Map.insert name True scope : scopes
define _ _                   = undefined

declareDefine :: String' -> [Scope] -> Either ResolveError [Scope]
declareDefine name = declare name >=> pure . (define . fst) name

calcDistance :: String -> [Scope] -> Maybe Int
calcDistance = List.findIndex . Map.member

getDistance :: String' -> Distances -> Either RuntimeError Int
getDistance name dists = case Map.lookup name dists of
  Just dist -> Right dist
  Nothing   -> Left $ RuntimeError "Unresolved variable" `uncurry` name
