module Resolver where

import Data.Map (Map, empty, insert)

import AST
import Error    (ResolveError)

-- Scope

type Scope = Map String Bool

begin :: [Scope] -> [Scope]
begin stack = empty : stack

declare :: String -> [Scope] -> [Scope]
declare name stack = case stack of
  (scope : scopes) -> insert name False scope : scopes
  []               -> stack

define :: String -> [Scope] -> [Scope]
define name stack = case stack of
  (scope : scopes) -> insert name True scope : scopes
  []               -> stack

decDef :: String -> [Scope] -> [Scope]
decDef name' stack' = define name' $ declare name' stack'

-- Resolve

resolve :: [Stmt] -> Either ResolveError [Stmt]
resolve stmts = resolveStmts stmts [empty] >> Right stmts

resolveStmts :: [Stmt] -> [Scope] -> Either ResolveError [Scope]
resolveStmts [] stack = Right stack
resolveStmts (stmt : stmts) stack = case stmt of
  Expr expr ->
    resolveExpr expr stack >>= resolveStmts stmts
  Print expr ->
    resolveExpr expr stack >>= resolveStmts stmts
  Block stmts' ->
    resolveStmts stmts' (begin stack) >> resolveStmts stmts stack
  Var name (Just expr) ->
    resolveExpr expr (declare name stack) >>= resolveStmts stmts . define name
  Var name Nothing ->
    resolveStmts stmts (decDef name stack)
  Return (Just expr) ->
    resolveExpr expr stack >>= resolveStmts stmts
  Return Nothing ->
    resolveStmts stmts stack
  If cond thenStmt elseStmt -> do
    stack' <- resolveExpr cond stack >>= resolveStmts [thenStmt]
    case elseStmt of
      Just stmt' -> resolveStmts [stmt'] stack' >>= resolveStmts stmts
      Nothing    -> resolveStmts stmts stack'
  While cond stmt' -> do
    resolveExpr cond stack >>= resolveStmts [stmt'] >>= resolveStmts stmts
  Fun name params stmts' -> do
    let stack1 = decDef name stack
    let stack2 = foldr decDef (begin stack1) params
    resolveStmts stmts' stack2 >> resolveStmts stmts stack

resolveExpr :: Expr -> [Scope] -> Either ResolveError [Scope]
resolveExpr _ = Right
