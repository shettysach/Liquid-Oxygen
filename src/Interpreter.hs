{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections       #-}

module Interpreter where

import Control.Arrow              (first, second)
import Control.Monad              (foldM, void)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT, throwE)
import Data.Functor               ((<&>))

import Data.Map                   qualified as Map
import Environment
import Error                      (RuntimeError (RuntimeError))
import Syntax

interpret :: ([Stmt], Distances) -> IO (Either RuntimeError ())
interpret (statements, distances) = void <$> runExceptT (interpretStmts statements distances global)

interpretStmts :: [Stmt] -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
interpretStmts [] _ env = pure (Nil, env)
interpretStmts (stmt : stmts) dists env = case stmt of
  Expr expr -> evaluate expr dists env >>= interpretStmts stmts dists . snd
  Var var (Just expr) -> evaluate expr dists env >>= interpretStmts stmts dists . uncurry (initialize $ fst var)
  Var var Nothing -> interpretStmts stmts dists $ initialize (fst var) Nil env
  Return (Just expr, _) -> evaluate expr dists env <&> second parent
  Return (Nothing, _) -> evaluate (Literal Nil) dists env <&> second parent
  Block stmts' -> interpretStmts stmts' dists (child env) >>= interpretStmts stmts dists . parent . snd
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
  Function{} ->
    let closure = initialize fname fun env
        (fun, fname) = interpretFunction stmt dists closure
     in interpretStmts stmts dists closure
  Class name (Just super) methods -> do
    (literal, env') <- evaluate super dists env
    super' <- case literal of
      Class'{} -> pure $ Just literal
      _        -> throwE $ RuntimeError "Superclass must be a class" `uncurry` name
    let envS = initialize "super" literal $ child env'
    let klass = Class' name super' (mapMethods methods dists envS Map.empty)
    interpretStmts stmts dists $ initialize (fst name) klass env'
  Class name Nothing methods ->
    let klass = Class' name Nothing (mapMethods methods dists env Map.empty)
     in interpretStmts stmts dists $ initialize (fst name) klass env

interpretFunction :: Stmt -> Distances -> Env -> (Literal, String)
interpretFunction (Function name params stmts') dists closure = do
  let callable args env =
        let envF = foldr (uncurry initialize) (child env) (zip (map fst params) args)
         in runExceptT (interpretStmts stmts' dists envF)
   in (Function' name callable (length params) closure, fst name)
interpretFunction _ _ _ = undefined

mapMethods :: [Stmt] -> Distances -> Env -> Map.Map String Literal -> Map.Map String Literal
mapMethods [] _ _ mmap = mmap
mapMethods (mthd : mthds) dists closure mmap =
  let (fun, fname) = interpretFunction mthd dists closure
      mmap' = Map.insert fname fun mmap
   in mapMethods mthds dists closure mmap'

evaluate :: Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
evaluate (Literal lit) _ env = except $ Right (lit, env)
evaluate (Grouping expr) dists env = evaluate expr dists env
evaluate (Variable var) dists env = except $ getAt var dists env <&> (,env)
evaluate (Assignment var expr) dists env = do
  (lit, env') <- evaluate expr dists env
  except $ getDistance var dists >>= assignAt var lit `flip` env' <&> (lit,)
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
  (args', closure') <- foldM evalArg ([], closure) args
  visitCall (snd callee) callee' args' closure' <&> (,env')
evaluate (Get expr prop) dists env = do
  (inst, envI) <- evaluate expr dists env
  except $ case inst of
    Instance' _ super props -> case Map.lookup (fst prop) props of
      Just lit -> case lit of
        Function' name fn arity closure ->
          let closure' = initialize "this" inst $ child closure
              bound = Function' name fn arity closure'
           in Right (bound, envI)
        _ -> Right (lit, envI)
      Nothing -> superLookup super prop inst envI
    _ -> Left $ RuntimeError "Only instances have properties/methods" `uncurry` prop
evaluate (Set expr prop expr') dists env = do
  (inst, envI) <- evaluate expr dists env
  (lit, envL) <- evaluate expr' dists envI
  except $ do
    inst' <- case inst of
      Instance' name super props -> let props' = Map.insert (fst prop) lit props in Right (Instance' name super props')
      _                          -> Left $ RuntimeError "Only instances have properties/methods" `uncurry` prop
    env' <- case expr of
      Variable var -> getDistance var dists >>= assignAt var inst' `flip` envL
      This pos     -> assignAt ("this", pos) inst' 1 envL
      _            -> pure envL
    Right (inst', env')
evaluate (This pos) dists env = except $ getAt ("this", pos) dists env <&> (,env)
evaluate (Super pos mthd) dists env = except $ do
  super <- getAt ("super", pos) dists env
  inst <- getHere ("this", pos) $ parent env
  methods <- case super of
    Class' _ _ methods -> Right methods
    _                  -> Left $ RuntimeError "Super must refer to a class" "super" pos
  case Map.lookup (fst mthd) methods of
    Just (Function' name fn arity closure) ->
      let closure' = initialize "this" inst $ child closure
          bound = Function' name fn arity closure'
       in Right (bound, env)
    _ -> Left $ RuntimeError "Undefined method" `uncurry` mthd

superLookup :: Maybe Literal -> String' -> Literal -> Env -> Either RuntimeError (Literal, Env)
superLookup (Just (Class' _ super props)) prop inst envI = case Map.lookup (fst prop) props of
  Just lit -> case lit of
    Function' name fn arity closure ->
      let closure' = initialize "this" inst $ child closure
          bound = Function' name fn arity closure'
       in Right (bound, envI)
    _ -> Right (lit, initialize "this" inst $ child envI)
  Nothing -> superLookup super prop inst envI
superLookup Nothing prop _ _ = Left $ RuntimeError "Undefined property/method" `uncurry` prop
superLookup _ _ _ _ = undefined

visitCall :: (Int, Int) -> Literal -> [Literal] -> Env -> ExceptT RuntimeError IO Literal
visitCall _ (Function' name fun arity _) args closure
  | length args == arity = ExceptT $ fmap fst <$> fun args closure
  | otherwise = throwE $ RuntimeError ("Arity /= " ++ show arity) `uncurry` name
visitCall _ (Class' name super methods) args closure =
  let instance' = Instance' name super methods
   in case Map.lookup "init" methods of
        Just (Function' _ initr arity _) | length args == arity -> ExceptT $ do
          result <- initr args (initialize "this" instance' closure)
          pure $ result >>= getHere ("this", snd name) . parent . snd
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
