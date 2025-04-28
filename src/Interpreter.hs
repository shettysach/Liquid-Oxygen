{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE TupleSections       #-}

module Interpreter where

import Control.Arrow              (first, second)
import Control.Monad              (void)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT, throwE)
import Data.Foldable              (foldlM, foldrM)
import Data.Functor               ((<&>))
import Data.Map                   qualified as Map

import Environment
import Error                      (RuntimeError (RuntimeError))
import Syntax

interpret :: ([Stmt], Distances) -> IO (Either RuntimeError ())
interpret (statements, distances) = global >>= runExceptT . interpretStmts statements distances <&> void

interpretStmts :: [Stmt] -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
interpretStmts [] _ env = pure (Nil, env)
interpretStmts (stmt : stmts) dists env = case stmt of
  Expr expr -> evaluate expr dists env >>= interpretStmts stmts dists . snd
  Var var (Just expr) ->
    evaluate expr dists env
      >>= liftIO . uncurry (initialize $ fst var)
      >>= interpretStmts stmts dists
  Var var Nothing -> liftIO (initialize (fst var) Nil env) >>= interpretStmts stmts dists
  Return (Just expr, _) -> evaluate expr dists env <&> second parent
  Return (Nothing, _) -> evaluate (Literal Nil) dists env <&> second parent
  Block stmts' ->
    liftIO (child env)
      >>= interpretStmts stmts' dists
      >>= interpretStmts stmts dists . parent . snd
  Print expr -> evaluate expr dists env >>= liftA2 (>>) (liftIO . print . fst) (interpretStmts stmts dists . snd)
  If cond thenStmt elseStmt -> do
    (cond', env') <- evaluate cond dists env
    if isTruthy cond'
      then interpretStmts (thenStmt : stmts) dists env'
      else case elseStmt of
        Just stmt' -> interpretStmts (stmt' : stmts) dists env'
        Nothing    -> interpretStmts stmts dists env'
  While cond stmt' ->
    let while env' = do
          (cond', envC) <- evaluate cond dists env'
          if isTruthy cond'
            then interpretStmts [stmt'] dists envC >>= while . snd
            else interpretStmts stmts dists envC
     in while env
  Function{} -> mdo
    closure <- lift (initialize name fun env)
    (name, fun) <- lift $ interpretFunction stmt dists closure
    interpretStmts stmts dists closure
  Class name (Just super) methods -> do
    (literal, env') <- evaluate super dists env
    super' <- case literal of
      Class'{} -> pure $ Just literal
      _        -> throwE $ RuntimeError "Superclass must be a class" `uncurry` name
    envS <- liftIO $ liftIO (child env) >>= initialize "super" literal
    methods' <- liftIO $ mapMethods methods dists envS Map.empty
    let klass = Class' name super' methods'
    liftIO (initialize (fst name) klass env')
      >>= interpretStmts stmts dists
  Class name Nothing methods -> do
    methods' <- liftIO $ mapMethods methods dists env Map.empty
    let klass = Class' name Nothing methods'
    env' <- liftIO $ initialize (fst name) klass env
    interpretStmts stmts dists env'

interpretFunction :: Stmt -> Distances -> Env -> IO (String, Literal)
interpretFunction (Function name params body) dists closure = do
  let callable args env =
        child env
          >>= foldrM (uncurry initialize) `flip` zip (map fst params) args
          >>= runExceptT . interpretStmts body dists
  pure (fst name, Function' name callable (length params) closure)
interpretFunction _ _ _ = undefined

mapMethods :: [Stmt] -> Distances -> Env -> Map.Map String Literal -> IO (Map.Map String Literal)
mapMethods [] _ _ mthds = pure mthds
mapMethods (m : ms) dists closure mthds = do
  (fun, name) <- interpretFunction m dists closure
  mapMethods ms dists closure (Map.insert fun name mthds)

evaluate :: Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
evaluate (Literal lit) _ env = except $ Right (lit, env)
evaluate (Grouping expr) dists env = evaluate expr dists env
evaluate (Variable var) dists env = ExceptT $ liftIO (getAt var dists env) <&> fmap (,env)
evaluate (Assignment var expr) dists env = do
  (lit, env') <- evaluate expr dists env
  dist <- except $ getDistance var dists
  liftIO (assignAt var lit dist env') >>= except
  pure (lit, env')
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
  let evalArg (lits, envA) arg = first (: lits) <$> evaluate arg dists envA
  let closure = case callee' of
        Function' _ _ _ clr -> clr
        _                   -> env
  foldlM evalArg ([], closure) args
    >>= uncurry (visitCall (snd callee) callee')
    <&> (,env')
evaluate (Get expr prop) dists env = do
  (inst, envI) <- evaluate expr dists env
  case inst of
    Instance' _ super props -> case Map.lookup (fst prop) props of
      Just lit -> case lit of
        Function' name fn arity closure -> do
          closure' <- liftIO $ child closure >>= initialize "this" inst
          let fun = Function' name fn arity closure'
          pure (fun, envI)
        _ -> pure (lit, envI)
      Nothing -> superLookup super prop inst envI
    _ -> throwE $ RuntimeError "Only instances have properties/methods" `uncurry` prop
evaluate (Set expr prop expr') dists env = do
  (inst, envI) <- evaluate expr dists env
  (lit, envL) <- evaluate expr' dists envI
  inst' <- case inst of
    Instance' name super props ->
      pure (Instance' name super (Map.insert (fst prop) lit props))
    _ -> throwE $ RuntimeError "Only instances have properties/methods" `uncurry` prop
  case expr of
    Variable var -> do
      dist <- except (getDistance var dists)
      liftIO (assignAt var inst' dist envL) >>= except
    This pos ->
      liftIO (assignAt ("this", pos) inst' 1 envL) >>= except
    _ -> pure ()
  pure (inst', envL)
evaluate (This pos) dists env = ExceptT $ liftIO $ getAt ("this", pos) dists env <&> fmap (,env)
evaluate (Super pos mthd) dists env = do
  super <- ExceptT $ getAt ("super", pos) dists env
  inst <- ExceptT $ getHere ("this", pos) $ parent env
  methods <- case super of
    Class' _ _ methods -> pure methods
    _                  -> throwE $ RuntimeError "Super must refer to a class" "super" pos
  case Map.lookup (fst mthd) methods of
    Just (Function' name fn arity closure) -> do
      closure' <- liftIO $ child closure >>= initialize "this" inst
      let bound = Function' name fn arity closure'
      pure (bound, env)
    _ -> throwE $ RuntimeError "Undefined method" `uncurry` mthd

superLookup :: Maybe Literal -> String' -> Literal -> Env -> ExceptT RuntimeError IO (Literal, Env)
superLookup (Just (Class' _ super props)) prop inst envI = case Map.lookup (fst prop) props of
  Just lit -> case lit of
    Function' name fn arity closure -> do
      closure' <- liftIO $ child closure >>= initialize "this" inst
      let bound = Function' name fn arity closure'
      pure (bound, envI)
    _ -> liftIO $ child envI >>= initialize "this" inst <&> (lit,)
  Nothing -> superLookup super prop inst envI
superLookup Nothing prop _ _ = throwE $ RuntimeError "Undefined property/method" `uncurry` prop
superLookup _ _ _ _ = error "superLookup: unexpected non-class super literal"

visitCall :: (Int, Int) -> Literal -> [Literal] -> Env -> ExceptT RuntimeError IO Literal
visitCall _ (Function' name fun arity _) args closure
  | length args == arity = ExceptT $ fmap fst <$> fun args closure
  | otherwise = throwE $ RuntimeError ("Arity /= " ++ show arity) `uncurry` name
visitCall _ (Class' name super methods) args closure = do
  let instance' = Instance' name super methods
  case Map.lookup "init" methods of
    Just (Function' _ initr arity _)
      | length args == arity ->
          liftIO (initialize "this" instance' closure)
            >>= (liftIO . initr args)
            >>= ExceptT . pure
            >>= (ExceptT . getHere ("this", snd name) . parent . snd)
    Nothing | null args -> pure instance'
    Just (Function' _ _ arity _) -> throwE $ RuntimeError ("Arity /= " ++ show arity) `uncurry` name
    Nothing -> throwE $ RuntimeError "Class const takes no arguments" `uncurry` name
    _ -> throwE $ RuntimeError "Invalid init in class defn" `uncurry` name
visitCall pos lit _ _ = throwE $ RuntimeError "Calling non-function/non-class" (show lit) pos

visitLogical :: LogicalOp' -> Expr -> Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
visitLogical op left right dists env = do
  result@(left', env') <- evaluate left dists env
  case fst op of
    Or | isTruthy left'          -> pure result
    And | not . isTruthy $ left' -> pure result
    _                            -> evaluate right dists env'

visitUnary :: UnaryOp' -> Literal -> Either RuntimeError Literal
visitUnary (Minus', _) (Number' n) = Right . Number' . negate $ n
visitUnary (Minus', pos) _         = Left $ RuntimeError "Invalid operand" (show Minus') pos
visitUnary _ right                 = Right . Bool' . not . isTruthy $ right

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
