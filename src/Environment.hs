{-# LANGUAGE ImportQualifiedPost #-}

module Environment where

import Control.Monad    ((>=>))
import Data.Functor     (($>))
import Data.IORef       (newIORef, readIORef, writeIORef)
import Data.List        qualified as List
import Data.Map         qualified as Map
import System.IO.Unsafe (unsafePerformIO)

import Error            (ResolveError (ResolveError), RuntimeError (RuntimeError))
import Syntax           (Env (Env), Literal (..), String')

global :: Env
global = Env Map.empty Nothing

{-# NOINLINE initialize #-}
initialize :: String -> Literal -> Env -> Env
initialize name value (Env scope prev) =
  let ref = unsafePerformIO (newIORef value)
      scope' = Map.insert name ref scope
   in Env scope' prev

getHere :: String' -> Env -> Either RuntimeError Literal
getHere (var, pos) (Env scope _) =
  case Map.lookup var scope of
    Just ref -> Right (unsafePerformIO (readIORef ref))
    Nothing  -> Left $ RuntimeError "Undefined variable" var pos

getAt :: String' -> Distances -> Env -> Either RuntimeError Literal
getAt name dists env = getDistance name dists >>= getHere name . ancestor env

assignAt :: String' -> Literal -> Int -> Env -> Either RuntimeError Env
assignAt (var, pos) value 0 env@(Env scope _)
  | Just ref <- Map.lookup var scope = unsafePerformIO (writeIORef ref value) `seq` Right env
  | otherwise = Left $ RuntimeError "Undefined variable" var pos
assignAt name value d (Env scope (Just up)) = assignAt name value (d - 1) up $> Env scope (Just up)
assignAt (var, pos) _ _ _ = Left $ RuntimeError "Undefined variable" var pos

child :: Env -> Env
child env = Env Map.empty (Just env)

parent :: Env -> Env
parent (Env _ (Just enc)) = enc
parent _                  = undefined

ancestor :: Env -> Int -> Env
ancestor env 0                = env
ancestor (Env _ (Just enc)) d = ancestor enc (d - 1)
ancestor _ _                  = undefined

--

type Scope = Map.Map String Bool
type Distances = Map.Map String' Int

begin :: [Scope] -> [Scope]
begin = (Map.empty :)

declare :: String' -> [Scope] -> Either ResolveError [Scope]
declare (var, pos) (scope : scopes)
  | Map.member var scope = Left $ ResolveError "Variable already declared" var pos
  | otherwise = Right $ Map.insert var False scope : scopes
declare _ [] = Right []

define :: String -> [Scope] -> [Scope]
define name (scope : scopes) = Map.insert name True scope : scopes
define _ []                  = []

declareDefine :: String' -> [Scope] -> Either ResolveError [Scope]
declareDefine name = declare name >=> Right . (define . fst) name

calcDistance :: String -> [Scope] -> Maybe Int
calcDistance = List.findIndex . Map.member

getDistance :: String' -> Distances -> Either RuntimeError Int
getDistance name dists = case Map.lookup name dists of
  Just dist -> Right dist
  Nothing   -> Left $ RuntimeError "Unresolved variable" `uncurry` name
