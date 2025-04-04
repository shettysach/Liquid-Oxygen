{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections       #-}

module Resolver where

import Control.Monad ((>=>))
import Data.Foldable (foldrM)
import Data.Map      qualified as Map

import Environment
import Error         (ResolveError (ResolveError))
import Syntax

-- https://hackage.haskell.org/package/extra-1.8/docs/Data-Tuple-Extra.html

first3 :: (a -> a') -> (a, b, c) -> (a', b, c)
first3 f ~(a, b, c) = (f a, b, c)

second3 :: (b -> b') -> (a, b, c) -> (a, b', c)
second3 f ~(a, b, c) = (a, f b, c)

third3 :: (c -> c') -> (a, b, c) -> (a, b, c')
third3 f ~(a, b, c) = (a, b, f c)

--

data FunctionType = None | Fun | Mthd
  deriving (Show)

type State = (FunctionType, Distances, [Scope])

resolve :: [Stmt] -> Either ResolveError ([Stmt], Distances)
resolve stmts = do
  (_, dists, _) <- resolveStmts stmts (None, Map.empty, [Map.empty])
  Right (stmts, dists)

resolveStmts :: [Stmt] -> State -> Either ResolveError State
resolveStmts [] state = Right state
resolveStmts (stmt : stmts) state@(ftype, dists, stack) = case stmt of
  Expr expr -> resolveExpr expr state >>= resolveStmts stmts
  Var name (Just expr) ->
    declare name stack
      >>= resolveExpr expr . (ftype,dists,)
      >>= resolveStmts stmts . third3 (define name)
  Var name Nothing -> declareDefine name stack >>= resolveStmts stmts . (ftype,dists,)
  Return expr | None <- ftype -> Left $ ResolveError "Top level return" "return" $ snd expr
  Return expr -> case fst expr of
    Just expr' -> resolveExpr expr' state >>= resolveStmts stmts
    Nothing    -> resolveStmts stmts state
  Block stmts' -> resolveStmts stmts' (ftype, dists, begin stack) >>= resolveStmts stmts . third3 tail
  Print expr -> resolveExpr expr state >>= resolveStmts stmts
  If cond thenStmt elseStmt ->
    resolveExpr cond state >>= resolveStmts [thenStmt] >>= case elseStmt of
      Just stmt' -> resolveStmts [stmt'] >=> resolveStmts stmts
      Nothing    -> resolveStmts stmts
  While cond stmt' -> resolveExpr cond state >>= resolveStmts [stmt'] >>= resolveStmts stmts
  Function{} -> resolveFunctions stmts stmt Fun state
  Class name methods ->
    declareDefine name (define ("this", snd name) stack)
      >>= (foldrM (resolveFunctions stmts `flip` Mthd) `flip` methods) . (ftype,dists,) . begin
      >>= resolveStmts stmts . first3 (const ftype) . third3 tail

resolveFunctions :: [Stmt] -> Stmt -> FunctionType -> State -> Either ResolveError State
resolveFunctions stmts (Function name params stmts' _) ntype (ftype, dists, stack) =
  foldrM declareDefine (begin $ define name stack) params
    >>= resolveStmts stmts' . (ntype,dists,)
    >>= resolveStmts stmts . first3 (const ftype) . third3 tail
resolveFunctions _ _ _ _ = undefined

resolveExpr :: Expr -> State -> Either ResolveError State
resolveExpr (Literal _) state             = Right state
resolveExpr (Grouping expr) state         = resolveExpr expr state
resolveExpr (Variable name) state         = resolveLocal name state
resolveExpr (Assignment name expr) state  = resolveExpr expr state >>= resolveLocal name
resolveExpr (Unary _ right) state         = resolveExpr right state
resolveExpr (Binary _ left right) state   = resolveExpr left state >>= resolveExpr right
resolveExpr (Logical _ left right) state  = resolveExpr left state >>= resolveExpr right
resolveExpr (Call (expr, _) args) state   = resolveExpr expr state >>= foldrM resolveExpr `flip` args
resolveExpr (Get expr _) state            = resolveExpr expr state
resolveExpr (Set expr _ expr') state      = resolveExpr expr state >>= resolveExpr expr'
resolveExpr (This pos) state@(Mthd, _, _) = resolveLocal ("this", pos) state
resolveExpr (This pos) _                  = Left $ ResolveError "Used `this` out of class" "this" pos

resolveLocal :: String' -> State -> Either ResolveError State
resolveLocal name (_, _, scope : _)
  | Map.lookup (fst name) scope == Just False =
      Left $ uncurry (ResolveError "Can't read local variable in own init") name
resolveLocal name state@(_, _, stack) =
  case calcDistance 0 name stack of
    Just dist -> Right $ second3 (Map.insert name dist) state
    Nothing   -> Right state
