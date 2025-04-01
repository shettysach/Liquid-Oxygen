{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections       #-}

module Resolver where

import Control.Arrow (second)
import Control.Monad ((>=>))
import Data.Foldable (foldrM)
import Data.Map      qualified as Map

import Environment
import Error         (ResolveError (ResolveError))
import Syntax

type State = (Distances, [Scope])

resolve :: [Stmt] -> Either ResolveError ([Stmt], Distances)
resolve stmts = (stmts,) . fst <$> resolveStmts stmts (Map.empty, [Map.empty])

resolveStmts :: [Stmt] -> State -> Either ResolveError State
resolveStmts [] state = Right state
resolveStmts (stmt : stmts) state@(dists, stack) = case stmt of
  Expr expr -> resolveExpr expr state >>= resolveStmts stmts
  Var name (Just expr) ->
    declare name stack
      >>= resolveExpr expr . (dists,)
      >>= resolveStmts stmts . second (define name)
  Var name Nothing -> declareDefine name stack >>= resolveStmts stmts . (dists,)
  Return (Just expr) -> resolveExpr expr state >>= resolveStmts stmts
  Return Nothing -> resolveStmts stmts state
  Block stmts' -> resolveStmts stmts' (dists, begin stack) >>= resolveStmts stmts . second tail
  Print expr -> resolveExpr expr (dists, stack) >>= resolveStmts stmts
  If cond thenStmt elseStmt ->
    resolveExpr cond state >>= resolveStmts [thenStmt] >>= case elseStmt of
      Just stmt' -> resolveStmts [stmt'] >=> resolveStmts stmts
      Nothing    -> resolveStmts stmts
  While cond stmt' -> resolveExpr cond state >>= resolveStmts [stmt'] >>= resolveStmts stmts
  Fun name params stmts' ->
    foldrM declareDefine (begin . define name $ stack) (reverse params)
      >>= resolveStmts stmts' . (dists,)
      >>= resolveStmts stmts . second tail

-- NOTE: second tail vs second (const stack)

resolveExpr :: Expr -> State -> Either ResolveError State
resolveExpr (Literal _) state            = Right state
resolveExpr (Grouping expr) state        = resolveExpr expr state
resolveExpr (Variable name) state        = resolveLocal name state
resolveExpr (Assignment name expr) state = resolveExpr expr state >>= resolveLocal name
resolveExpr (Unary _ right) state        = resolveExpr right state
resolveExpr (Binary _ left right) state  = resolveExpr left state >>= resolveExpr right
resolveExpr (Logical _ left right) state = resolveExpr left state >>= resolveExpr right
resolveExpr (Call expr args) state       = resolveExpr expr state >>= foldrM resolveExpr `flip` args

resolveLocal :: Name -> State -> Either ResolveError State
resolveLocal name (_, scope : _)
  | Map.lookup (fst name) scope == Just False =
      Left $ uncurry (ResolveError "Can't read local variable in own init") name
resolveLocal name (dists, stack) =
  case calcDistance 0 name stack of
    Just dist -> Right (Map.insert name dist dists, stack)
    Nothing   -> Right (dists, stack)
