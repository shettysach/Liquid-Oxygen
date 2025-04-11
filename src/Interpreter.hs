{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections       #-}

module Interpreter where

import Control.Arrow              (first, second)
import Control.Monad              (foldM, void)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT, throwE)
import Data.Functor               ((<&>))

import Data.Map                   qualified as Map
import Environment                as Env
import Error                      (RuntimeError (RuntimeError))
import Syntax

interpret :: ([Stmt], Distances) -> IO (Either RuntimeError ())
interpret (statements, distances) = void <$> runExceptT (interpretStmts statements distances global)

interpretStmts :: [Stmt] -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
interpretStmts [] _ env = pure (Nil, env)
interpretStmts (stmt : stmts) dists env = case stmt of
  Expr expr -> evaluate expr dists env >>= interpretStmts stmts dists . snd
  Var var (Just expr) -> evaluate expr dists env >>= interpretStmts stmts dists . uncurry (initialize $ fst var)
  Var var Nothing -> interpretStmts stmts dists (initialize (fst var) Nil env)
  Return mExpr -> case fst mExpr of
    Just expr -> evaluate expr dists env <&> second Env.parent
    Nothing   -> evaluate (Literal Nil) dists env <&> second Env.parent
  Block stmts' -> interpretStmts stmts' dists (Env.child env) >>= interpretStmts stmts dists . Env.parent . snd
  Print expr -> do
    (lit, env') <- evaluate expr dists env
    (liftIO . print) lit
    interpretStmts stmts dists env'
  If cond thenStmt elseStmt -> do
    (cond', env') <- evaluate cond dists env
    if isTruthy cond'
      then interpretStmts (thenStmt : stmts) dists env'
      else case elseStmt of
        Just stmt' -> interpretStmts (stmt' : stmts) dists env'
        Nothing    -> interpretStmts stmts dists env'
  While cond stmt' -> while env
   where
    while env' = do
      (cond', envC) <- evaluate cond dists env'
      if isTruthy cond'
        then interpretStmts [stmt'] dists envC >>= while . snd
        else interpretStmts stmts dists envC
  Function{} ->
    let (fun, fname) = interpretFunction stmt dists
     in interpretStmts stmts dists (initialize fname fun env)
  Class name (Just super) methods -> do
    (literal, env') <- evaluate super dists env
    super' <- case (literal, super) of
      (Class'{}, _)     -> pure $ Just literal
      (_, Variable var) -> throwE $ RuntimeError "Superclass must be class" `uncurry` var
      _                 -> undefined
    let klass = Class' name super' (mapMthds methods dists Map.empty)
     in interpretStmts stmts dists (initialize (fst name) klass env')
  Class name Nothing methods ->
    let klass = Class' name Nothing (mapMthds methods dists Map.empty)
     in interpretStmts stmts dists (initialize (fst name) klass env)

mapMthds :: [Stmt] -> Distances -> Map.Map String Literal -> Map.Map String Literal
mapMthds [] _ mmap = mmap
mapMthds (mthd : mthds) dists mmap =
  let (fun, fname) = interpretFunction mthd dists
      mmap' = Map.insert fname fun mmap
   in mapMthds mthds dists mmap'

interpretFunction :: Stmt -> Distances -> (Literal, String)
interpretFunction (Function name params stmts') dists = do
  let callable args env' =
        let envF = foldr (uncurry initialize) (Env.child env') (zip (map fst params) args)
         in runExceptT (interpretStmts stmts' dists envF)
   in (Function' name callable (length params), fst name)
interpretFunction _ _ = undefined

evaluate :: Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
evaluate (Literal lit) _ env = except $ Right (lit, env)
evaluate (Grouping expr) dists env = evaluate expr dists env
evaluate (Variable var) dists env = except $ Env.get var (resolveEnv var dists env) <&> (,env)
evaluate (Assignment var expr) dists env = do
  (lit, env') <- evaluate expr dists env
  except $ Env.getDistance var dists >>= Env.assign var lit `flip` env' <&> (lit,)
evaluate (Unary op right) dists env = do
  (r, envR) <- evaluate right dists env
  except $ visitUnary op r <&> (,envR)
evaluate (Binary op left right) dists env = do
  (l, envL) <- evaluate left dists env
  (r, envR) <- evaluate right dists envL
  except $ visitBinary op l r <&> (,envR)
evaluate (Logical op left right) dists env =
  visitLogical op left right dists env
evaluate (Call callee args) dists env = do
  (callee', env') <- evaluate (fst callee) dists env
  let closure = case callee' of
        Function' name _ _ -> resolveEnv name dists env'
        Class' name _ _    -> resolveEnv name dists env'
        _                  -> undefined
      evalArg (lits, envA) arg = first (: lits) <$> evaluate arg dists envA
  (args', closure') <- foldM evalArg ([], closure) args
  let litT = visitCall (snd callee) callee' args' closure'
  litT <&> (,env')
evaluate (Get expr prop) dists env = do
  (inst, envI) <- evaluate expr dists env
  except $ case inst of
    Instance' _ super props -> case Map.lookup (fst prop) props of
      Just lit -> let envT = Env.child $ initialize "this" inst envI in Right (lit, parent envT)
      Nothing -> case super of
        Just (Class' _ _ props')
          | Just lit <- Map.lookup (fst prop) props' ->
              let envT = Env.child $ initialize "this" inst envI in Right (lit, parent envT)
        Nothing -> Left $ uncurry (RuntimeError "Undefined property/method") prop
        _ -> undefined
    _ -> Left $ RuntimeError "Only instances have properties/methods" `uncurry` prop
evaluate (Set expr prop expr') dists env = do
  (inst, envI) <- evaluate expr dists env
  (lit, envL) <- evaluate expr' dists envI
  except $ do
    inst' <- case inst of
      Instance' name super props -> let props' = Map.insert (fst prop) lit props in Right (Instance' name super props')
      _                          -> Left $ RuntimeError "Only instances have properties/methods" `uncurry` prop
    env' <- case expr of
      Variable var -> Env.getDistance var dists >>= Env.assign var inst' `flip` envL
      This pos     -> Env.assign ("this", pos) inst' 1 envL
      _            -> pure envL
    Right (inst', env')
evaluate (This pos) dists env =
  let this = ("this", pos) in except $ Env.get this (resolveEnv this dists env) <&> (,env)

visitCall :: (Int, Int) -> Literal -> [Literal] -> Env -> ExceptT RuntimeError IO Literal
visitCall _ (Function' name fun arity) args closure
  | length args == arity = ExceptT $ fmap fst <$> fun args closure
  | otherwise = throwE $ RuntimeError ("Arity /= " ++ show arity) `uncurry` name
visitCall _ (Class' name super methods) args closure =
  let instance' = Instance' name super methods
   in case Map.lookup "init" methods of
        Just (Function' _ initr arity) | length args == arity -> ExceptT $ do
          result <- initr args (Env.initialize "this" instance' closure)
          pure $ result >>= Env.get ("this", snd name) . parent . snd
        Nothing | null args -> pure instance'
        Just (Function' _ _ arity) -> throwE $ RuntimeError ("Arity /= " ++ show arity) `uncurry` name
        Nothing -> throwE $ RuntimeError "Class const takes no arguments" `uncurry` name
        _ -> throwE $ RuntimeError "Invalid init in class defn" `uncurry` name
visitCall pos lit _ _ = throwE $ RuntimeError "Calling non-function/non-class" (show lit) pos

visitLogical ::
  LogicalOp' -> Expr -> Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
visitLogical op left right dists env = do
  (left', env') <- evaluate left dists env
  case fst op of
    Or | isTruthy left'          -> pure (left', env')
    And | (not . isTruthy) left' -> pure (left', env')
    _                            -> evaluate right dists env'

visitUnary :: UnaryOp' -> Literal -> Either RuntimeError Literal
visitUnary op right
  | Minus' <- fst op = case right of
      Number' n -> Right $ Number' (-n)
      _         -> Left $ RuntimeError "Invalid operand" (show Minus') (snd op)
  | otherwise = (Right . Bool' . not . isTruthy) right

visitBinary :: BinaryOp' -> Literal -> Literal -> Either RuntimeError Literal
visitBinary op left right
  | EqualEqual <- fst op = Right $ Bool' (left == right)
  | BangEqual <- fst op = Right $ Bool' (left /= right)
  | otherwise = case (left, right) of
      (Number' l, Number' r) -> case fst op of
        Minus        -> Right $ Number' $ l - r
        Slash        -> Right $ Number' $ l / r
        Star         -> Right $ Number' $ l * r
        Plus         -> Right $ Number' $ l + r
        Greater      -> Right $ Bool' $ l > r
        GreaterEqual -> Right $ Bool' $ l >= r
        Less         -> Right $ Bool' $ l < r
        LessEqual    -> Right $ Bool' $ l <= r
      (String' l, String' r) -> case fst op of
        Plus -> Right $ String' $ l ++ r
        _    -> Left invalid
      _ -> Left invalid
 where
  invalid = RuntimeError "Invalid operands" (show $ fst op) (snd op)
