{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module Resolver where

import Control.Arrow       ((&&&))
import Control.Lens
import Control.Monad       ((>=>))
import Data.Foldable       (foldrM)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty  (NonEmpty ((:|)))
import Data.Maybe          (isJust)

import Environment
import Error               (ResolveError (ResolveError))
import Syntax

data FunctionType = NonF | Fun | Mthd | Init

data ClassType = NonC | Sup | Sub

type ResState = (FunctionType, ClassType, Distances, NonEmpty Scope)

resolve :: [Stmt] -> Either ResolveError Distances
resolve stmts = view dists <$> resolveStmts stmts (NonF, NonC, HashMap.empty, start)

replResolve :: [Stmt] -> NonEmpty Scope -> Either ResolveError (Distances, NonEmpty Scope)
replResolve stmts stack_ = (view dists &&& view stack) <$> resolveStmts stmts (NonF, NonC, HashMap.empty, stack_)

resolveStmts :: [Stmt] -> ResState -> Either ResolveError ResState
resolveStmts [] state = Right state
resolveStmts (stmt : stmts) state = case stmt of
  Expr expr -> resolveExpr expr state >>= resolveStmts stmts
  Var name (Just expr) ->
    declare name (view stack state)
      >>= resolveExpr expr . flip (set stack) state
      >>= resolveStmts stmts . over stack (define $ fst name)
  Var name Nothing -> declareDefine name (view stack state) >>= resolveStmts stmts . flip (set stack) state
  Return mExpr | NonF <- view ftype state -> Left $ ResolveError "Top level return" ("return", snd mExpr)
  Return mExpr | Init <- view ftype state, isJust $ fst mExpr -> Left $ ResolveError "Can't return value from init" ("return", snd mExpr)
  Return (Just expr, _) -> resolveExpr expr state >>= resolveStmts stmts
  Return (Nothing, _) -> resolveStmts stmts state
  Block stmts' -> resolveStmts stmts' (over stack begin state) >>= resolveStmts stmts . over stack end
  Print expr -> resolveExpr expr state >>= resolveStmts stmts
  If cond thenStmt elseStmt ->
    resolveExpr cond state
      >>= resolveStmts [thenStmt]
      >>= case elseStmt of
        Just stmt' -> resolveStmts [stmt'] >=> resolveStmts stmts
        Nothing    -> resolveStmts stmts
  While cond stmt' ->
    resolveExpr cond state
      >>= resolveStmts [stmt']
      >>= resolveStmts stmts
  Function fn | NonF <- view ftype state -> resolveFunction fn Fun state >>= resolveStmts stmts
  Function fn -> resolveFunction fn (view ftype state) state >>= resolveStmts stmts
  Class name (Just (Variable name')) _ | fst name == fst name' -> Left $ ResolveError "Can't inherit from self" name'
  Class name (Just super) methods ->
    declareDefine name (view stack state)
      >>= resolveExpr super . (view ftype state,Sub,view dists state,)
      <&> over stack (define "this" . begin . define "super" . begin)
      >>= foldrM resolveMethod `flip` methods
      >>= resolveStmts stmts . set ctype (view ctype state) . over stack (end . end)
  Class name Nothing methods ->
    declareDefine name (view stack state)
      <&> (view ftype state,Sup,view dists state,) . define "this" . begin
      >>= foldrM resolveMethod `flip` methods
      >>= resolveStmts stmts . set ctype (view ctype state) . over stack end

resolveFunction :: CompFn -> FunctionType -> ResState -> Either ResolveError ResState
resolveFunction (CompFn name params stmts) ntype state =
  declareDefine name (view stack state)
    >>= flip (foldrM declareDefine) params . begin
    >>= resolveStmts stmts . (ntype,view ctype state,view dists state,)
    <&> (over ftype . const) (view ftype state) . over stack end

resolveMethod :: CompFn -> ResState -> Either ResolveError ResState
resolveMethod mthd@(CompFn (fst -> "init") _ _) = resolveFunction mthd Init
resolveMethod mthd                              = resolveFunction mthd Mthd

resolveExpr :: Expr -> ResState -> Either ResolveError ResState
resolveExpr (Literal _) state                     = Right state
resolveExpr (Grouping expr) state                 = resolveExpr expr state
resolveExpr (Variable name) state                 = resolveLocal name state
resolveExpr (Assignment name expr) state          = resolveExpr expr state >>= resolveLocal name
resolveExpr (Unary _ right) state                 = resolveExpr right state
resolveExpr (Binary _ left right) state           = resolveExpr left state >>= resolveExpr right
resolveExpr (Logical _ left right) state          = resolveExpr left state >>= resolveExpr right
resolveExpr (Call (expr, _) args) state           = resolveExpr expr state >>= foldrM resolveExpr `flip` args
resolveExpr (Get expr _) state                    = resolveExpr expr state
resolveExpr (Set expr _ expr') state              = resolveExpr expr state >>= resolveExpr expr'
resolveExpr (This pos) state@(view ftype -> Mthd) = resolveLocal ("this", pos) state
resolveExpr (This pos) state@(view ftype -> Init) = resolveLocal ("this", pos) state
resolveExpr (This pos) _                          = Left $ ResolveError "`this` out of class" ("this", pos)
resolveExpr (Super pos _) (view ctype -> NonC)    = Left $ ResolveError "`super` out of class" ("super", pos)
resolveExpr (Super pos _) (view ctype -> Sup)     = Left $ ResolveError "`super` in class without superclass" ("super", pos)
resolveExpr (Super pos mthd) state                = resolveLocal ("super", pos) state >>= resolveLocal mthd

resolveLocal :: String' -> ResState -> Either ResolveError ResState
resolveLocal name state
  | scope :| _ <- view stack state
  , Just False <- HashMap.lookup (fst name) scope =
      Left $ ResolveError "Can't read local variable in own init" name
resolveLocal name state = pure $ case calcDistance (fst name) (view stack state) of
  Just dist -> over dists (HashMap.insert name dist) state
  Nothing   -> state

ftype :: Lens' (FunctionType, ClassType, Distances, NonEmpty Scope) FunctionType
ftype f (a, b, c, d) = (,b,c,d) <$> f a

ctype :: Lens' (FunctionType, ClassType, Distances, NonEmpty Scope) ClassType
ctype f (a, b, c, d) = (a,,c,d) <$> f b

dists :: Lens' (FunctionType, ClassType, Distances, NonEmpty Scope) Distances
dists f (a, b, c, d) = (a,b,,d) <$> f c

stack :: Lens' (FunctionType, ClassType, Distances, NonEmpty Scope) (NonEmpty Scope)
stack f (a, b, c, d) = (a,b,c,) <$> f d
