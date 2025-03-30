{-# LANGUAGE ImportQualifiedPost #-}

module Resolver where

import Control.Arrow (second)
import Data.Foldable (foldrM)
import Data.Map      qualified as Map

import Environment
import Error         (ResolveError (ResolveError))
import Syntax

type State = (Distances, [Scope])

resolve :: [Stmt] -> Either ResolveError ([Stmt], Distances)
resolve stmts = do
  (dists, _) <- resolveStmts stmts (Map.empty, [Map.empty])
  Right (stmts, dists)

resolveStmts :: [Stmt] -> State -> Either ResolveError State
resolveStmts [] state = Right state
resolveStmts (stmt : stmts) state@(dists, stack) = case stmt of
  Expr expr -> resolveExpr expr state >>= resolveStmts stmts
  Var name (Just expr) -> resolveExpr expr (dists, declare name stack) >>= resolveStmts stmts . second (define name)
  Var name Nothing -> resolveStmts stmts (dists, declareDefine name stack)
  Return (Just expr) -> resolveExpr expr state >>= resolveStmts stmts
  Return Nothing -> resolveStmts stmts state
  Block stmts' -> resolveStmts stmts' (dists, begin stack) >>= resolveStmts stmts . second tail
  Print expr -> resolveExpr expr (dists, stack) >>= resolveStmts stmts
  If cond thenStmt elseStmt -> do
    stack' <- resolveExpr cond state >>= resolveStmts [thenStmt]
    case elseStmt of
      Just stmt' -> resolveStmts [stmt'] stack' >>= resolveStmts stmts
      Nothing    -> resolveStmts stmts stack'
  While cond stmt' -> resolveExpr cond state >>= resolveStmts [stmt'] >>= resolveStmts stmts
  Fun name params stmts' ->
    let stack1 = declareDefine (fst name) stack
        stack2 = foldr declareDefine (begin stack1) params
     in resolveStmts stmts' (dists, stack2) >>= resolveStmts stmts . second tail

resolveExpr :: Expr -> State -> Either ResolveError State
resolveExpr (Literal _) state = Right state
resolveExpr (Grouping expr) state = resolveExpr expr state
resolveExpr (Variable var) (_, scope : _)
  | Map.lookup (fst var) scope == Just False =
      Left $ uncurry (ResolveError "Can't read local var in own init") var
resolveExpr (Variable var) state = resolveLocal var state
resolveExpr (Assignment name expr) state = resolveExpr expr state >>= resolveLocal name
resolveExpr (Unary _ right) state = resolveExpr right state
resolveExpr (Binary _ left right) state = resolveExpr left state >>= resolveExpr right
resolveExpr (Logical _ left right) state = resolveExpr left state >>= resolveExpr right
resolveExpr (Call callee args) state = resolveExpr callee state >>= foldrM resolveExpr `flip` args

resolveLocal :: Name -> State -> Either ResolveError State
resolveLocal name (dists, stack) =
  case calculate 0 name stack of
    Just dist -> Right (Map.insert name dist dists, stack)
    Nothing   -> Right (dists, stack)
 where
  calculate dist name' stack' = case stack' of
    scope : scopes
      | Map.member (fst name') scope -> Just dist
      | otherwise -> calculate (dist + 1) name' scopes
    [] -> Nothing
