{-# LANGUAGE ImportQualifiedPost #-}

module Resolver where

import Data.Foldable (foldrM)
import Data.Map      qualified as Map

import AST
import Environment   (Distances)
import Error         (RuntimeError (RuntimeError))

-- Scope

type Scope = Map.Map String Bool

begin :: [Scope] -> [Scope]
begin stack = Map.empty : stack

declare :: String -> [Scope] -> [Scope]
declare name stack = case stack of
  scope : scopes -> Map.insert name False scope : scopes
  []             -> undefined

define :: String -> [Scope] -> [Scope]
define name stack = case stack of
  scope : scopes -> Map.insert name True scope : scopes
  []             -> undefined

decDef :: String -> [Scope] -> [Scope]
decDef name' = define name' . declare name'

-- Resolve

resolve :: [Stmt] -> Either RuntimeError [Stmt]
resolve stmts = resolveStmts stmts [Map.empty] >> Right stmts

resolveStmts :: [Stmt] -> [Scope] -> Either RuntimeError [Scope]
resolveStmts [] stack = Right stack
resolveStmts (stmt : stmts) stack = case stmt of
  Expr expr ->
    resolveExpr expr stack >>= resolveStmts stmts
  Var name (Just expr) ->
    resolveExpr expr (declare name stack) >>= resolveStmts stmts . define name
  Var name Nothing ->
    resolveStmts stmts (decDef name stack)
  Return (Just expr) ->
    resolveExpr expr stack >>= resolveStmts stmts
  Return Nothing ->
    resolveStmts stmts stack
  Block stmts' ->
    resolveStmts stmts' (begin stack) >> resolveStmts stmts stack
  Print expr ->
    resolveExpr expr stack >>= resolveStmts stmts
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

resolveExpr :: Expr -> [Scope] -> Either RuntimeError [Scope]
resolveExpr (Literal _) stack = Right stack
resolveExpr (Grouping expr) stack =
  resolveExpr expr stack
resolveExpr (Variable name) stack =
  case stack of
    scope : _
      | Map.lookup (fst name) scope == Just False ->
          Left $ uncurry (RuntimeError "Can't read local var in own init") name
    _ -> do
      resolveLocal (fst name) stack
resolveExpr (Assignment name expr) stack =
  resolveExpr expr stack >>= resolveLocal (fst name)
resolveExpr (Unary _ right) stack =
  resolveExpr right stack
resolveExpr (Binary _ left right) stack =
  resolveExpr left stack >>= resolveExpr right
resolveExpr (Logical _ left right) stack =
  resolveExpr left stack >>= resolveExpr right
resolveExpr (Call callee args) stack = do
  stack' <- resolveExpr callee stack
  foldrM resolveExpr stack' args

resolveLocal :: String -> [Scope] -> Either RuntimeError [Scope]
resolveLocal name stack = do
  let dist = distance 0 name stack
  Left $ RuntimeError "idk" "idk" (0, 0)

distance :: Int -> String -> [Scope] -> Int
distance dist name stack = case stack of
  scope : scopes
    | Map.member name scope -> dist
    | otherwise -> distance (dist + 1) name scopes
  _ -> undefined
