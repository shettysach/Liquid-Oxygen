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

data FunctionType = None | Fun

resolve :: [Stmt] -> Either ResolveError ([Stmt], Distances)
resolve stmts = (stmts,) . fst <$> resolveStmts stmts None (Map.empty, [Map.empty])

resolveStmts :: [Stmt] -> FunctionType -> State -> Either ResolveError State
resolveStmts [] _ state = Right state
resolveStmts (stmt : stmts) ftype state@(dists, stack) = case stmt of
  Expr expr -> resolveExpr expr state >>= resolveStmts stmts ftype
  Var name (Just expr) ->
    declare name stack
      >>= resolveExpr expr . (dists,)
      >>= resolveStmts stmts ftype . second (define name)
  Var name Nothing -> declareDefine name stack >>= resolveStmts stmts ftype . (dists,)
  Return expr | None <- ftype -> Left $ ResolveError "Top level return" "return" (snd expr)
  Return expr -> case fst expr of
    Just expr' -> resolveExpr expr' state >>= resolveStmts stmts ftype
    Nothing    -> resolveStmts stmts ftype state
  Block stmts' -> resolveStmts stmts' ftype (dists, begin stack) >>= resolveStmts stmts ftype . second tail
  Print expr -> resolveExpr expr (dists, stack) >>= resolveStmts stmts ftype
  If cond thenStmt elseStmt ->
    resolveExpr cond state >>= resolveStmts [thenStmt] ftype >>= case elseStmt of
      Just stmt' -> resolveStmts [stmt'] ftype >=> resolveStmts stmts ftype
      Nothing    -> resolveStmts stmts ftype
  While cond stmt' -> resolveExpr cond state >>= resolveStmts [stmt'] ftype >>= resolveStmts stmts ftype
  Function name params stmts' _ ->
    foldrM declareDefine (begin $ define name stack) (reverse params)
      >>= resolveStmts stmts' Fun . (dists,)
      >>= resolveStmts stmts None . second tail
  Class name _mthds -> declareDefine name stack >>= resolveStmts stmts ftype . (dists,)

resolveExpr :: Expr -> State -> Either ResolveError State
resolveExpr (Literal _) state            = Right state
resolveExpr (Grouping expr) state        = resolveExpr expr state
resolveExpr (Variable name) state        = resolveLocal name state
resolveExpr (Assignment name expr) state = resolveExpr expr state >>= resolveLocal name
resolveExpr (Unary _ right) state        = resolveExpr right state
resolveExpr (Binary _ left right) state  = resolveExpr left state >>= resolveExpr right
resolveExpr (Logical _ left right) state = resolveExpr left state >>= resolveExpr right
resolveExpr (Call (expr, _) args) state  = resolveExpr expr state >>= foldrM resolveExpr `flip` args
resolveExpr (Get expr _) state           = resolveExpr expr state
resolveExpr (Set expr _ expr') state     = resolveExpr expr state >>= resolveExpr expr'

resolveLocal :: String' -> State -> Either ResolveError State
resolveLocal name (_, scope : _)
  | Map.lookup (fst name) scope == Just False =
      Left $ uncurry (ResolveError "Can't read local variable in own init") name
resolveLocal name (dists, stack) =
  case calcDistance 0 name stack of
    Just dist -> Right (Map.insert name dist dists, stack)
    Nothing   -> Right (dists, stack)
