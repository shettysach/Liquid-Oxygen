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

import Data.IORef                 (modifyIORef, newIORef, readIORef)
import Environment
import Error                      (RuntimeError (RuntimeError))
import Syntax

-- Stmts

interpret :: ([Stmt], Distances) -> IO (Either RuntimeError ())
interpret (statements, distances) = global >>= runExceptT . interpretStmts statements distances <&> void

replInterpret :: ([Stmt], Distances) -> Env -> IO (Either RuntimeError Env)
replInterpret (statements, distances) env = runExceptT (interpretStmts statements distances env) <&> fmap snd

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
  Print expr -> do
    (literal, env') <- evaluate expr dists env
    liftIO $ print literal
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
  Function{} -> mdo
    closure <- lift $ initialize name func env
    (name, func) <- lift $ interpretFunction stmt dists closure
    interpretStmts stmts dists closure
  Class name (Just super) methods -> do
    (literal, env') <- evaluate super dists env
    super' <- case literal of
      Class'{} -> pure $ Just literal
      _        -> throwE $ RuntimeError "Superclass must be a class" name
    envS <- liftIO $ liftIO (child env) >>= initialize "super" literal
    methods' <- liftIO $ mapMethods methods dists envS Map.empty
    let klass = Class' name super' methods'
    envC <- liftIO (initialize (fst name) klass env')
    interpretStmts stmts dists envC
  Class name Nothing methods -> do
    methods' <- liftIO $ mapMethods methods dists env Map.empty
    let klass = Class' name Nothing methods'
    envC <- liftIO (initialize (fst name) klass env)
    interpretStmts stmts dists envC

interpretFunction :: Stmt -> Distances -> Env -> IO (String, Literal)
interpretFunction (Function name params body) dists closure =
  let callable args env =
        child env
          >>= foldrM (uncurry initialize) `flip` zip (map fst params) args
          >>= runExceptT . interpretStmts body dists
   in pure (fst name, Function' name callable (length params) closure)
interpretFunction _ _ _ = undefined

mapMethods :: [Stmt] -> Distances -> Env -> Map.Map String Literal -> IO (Map.Map String Literal)
mapMethods [] _ _ mthds = pure mthds
mapMethods (m : ms) dists closure mthds = do
  (func, name) <- interpretFunction m dists closure
  mapMethods ms dists closure $ Map.insert func name mthds

-- Exprs

evaluate :: Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
evaluate expr dists env = case expr of
  Literal lit -> pure (lit, env)
  Grouping expr' -> evaluate expr' dists env
  Variable var -> ExceptT $ liftIO (getAt var dists env) <&> fmap (,env)
  Assignment var expr' -> do
    (literal, env') <- evaluate expr' dists env
    dist <- except $ getDistance var dists
    liftIO (assignAt var literal dist env')
      >> pure (literal, env')
  Unary op right -> do
    (r, envR) <- evaluate right dists env
    except $ visitUnary op r <&> (,envR)
  Binary op left right -> do
    (l, envL) <- evaluate left dists env
    (r, envR) <- evaluate right dists envL
    except $ visitBinary op l r <&> (,envR)
  Logical op left right -> visitLogical op left right dists env
  Call (callee, pos) argExprs -> do
    (calleeLit, envC) <- evaluate callee dists env
    let evalArg (vals, envV) arg = first (: vals) <$> evaluate arg dists envV
    argLits <- fst <$> foldlM evalArg ([], envC) argExprs
    case calleeLit of
      Function'{}    -> callFunction calleeLit argLits <&> (,envC)
      Class'{}       -> callClass calleeLit argLits <&> (,envC)
      NativeFn n f a -> callFunction (Function' (n, pos) f a undefined) argLits <&> (,envC)
      literal        -> throwE $ RuntimeError "Calling non-function/non-class" (show literal, pos)
  Get instance' field -> do
    (inst, envI) <- evaluate instance' dists env
    case inst of
      Instance' _ super fieldsRef -> do
        fields <- liftIO $ readIORef fieldsRef
        case Map.lookup (fst field) fields of
          Just (Function' name fn arity closure) -> do
            closure' <- liftIO $ child closure >>= initialize "this" inst
            let method = Function' name fn arity closure' in pure (method, envI)
          Just property -> pure (property, envI)
          Nothing -> superLookup super field inst envI
      _ -> throwE $ RuntimeError "Only instances have properties/methods" field
  Set instance' field expr' -> do
    (inst, envI) <- evaluate instance' dists env
    (literal, envL) <- evaluate expr' dists envI
    case inst of
      Instance' _ _ fieldsRef -> liftIO $ modifyIORef fieldsRef $ Map.insert (fst field) literal
      _                       -> throwE $ RuntimeError "Only instances have properties/methods" field
    pure (literal, envL)
  This pos -> ExceptT $ liftIO (getAt ("this", pos) dists env) <&> fmap (,env)
  Super pos mthd -> do
    super <- ExceptT $ getAt ("super", pos) dists env
    inst <- ExceptT $ getHere ("this", pos) $ parent env
    superLookup (Just super) mthd inst env

superLookup :: Maybe Literal -> String' -> Literal -> Env -> ExceptT RuntimeError IO (Literal, Env)
superLookup (Just (Class' _ super fields)) field inst envI = case Map.lookup (fst field) fields of
  Just (Function' name fn arity closure) -> do
    closure' <- liftIO $ child closure >>= initialize "this" inst
    let method = Function' name fn arity closure' in pure (method, envI)
  Nothing -> superLookup super field inst envI
  _ -> undefined
superLookup Nothing field _ _ = throwE $ RuntimeError "Undefined field" field
superLookup _ _ _ _ = undefined

callFunction :: Literal -> [Literal] -> ExceptT RuntimeError IO Literal
callFunction (Function' name func arity closure) args =
  if length args == arity
    then ExceptT $ fmap fst <$> func args closure
    else throwE $ RuntimeError ("Arity /= " ++ show arity) name
callFunction _ _ = undefined

callClass :: Literal -> [Literal] -> ExceptT RuntimeError IO Literal
callClass (Class' name super methods) args = do
  instance' <- liftIO (newIORef methods) <&> Instance' name super
  case Map.lookup "init" methods of
    Just (Function' _ initr arity closure) -> initClass initr arity closure instance'
    Nothing -> do
      (literal, closure) <- superLookup super ("init", snd name) instance' undefined
      case literal of
        Function' _ initr arity _ -> initClass initr arity closure instance'
        _                         -> throwE $ RuntimeError "Invalid init in superclass" name
    _ -> throwE $ RuntimeError "Invalid init in class defn" name
 where
  initClass initr arity closure inst =
    if length args == arity
      then do
        envT <- liftIO $ child closure >>= initialize "this" inst
        envI <- liftIO (initr args envT) >>= except <&> snd
        ExceptT $ getHere ("this", snd name) (parent envI)
      else throwE $ RuntimeError ("Arity /= " ++ show arity) name
callClass _ _ = undefined

visitLogical :: LogicalOp' -> Expr -> Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
visitLogical op left right dists env = do
  result@(left', env') <- evaluate left dists env
  case fst op of
    Or | isTruthy left'        -> pure result
    And | not $ isTruthy left' -> pure result
    _                          -> evaluate right dists env'

visitUnary :: UnaryOp' -> Literal -> Either RuntimeError Literal
visitUnary (Minus', _) (Number' n) = Right $ Number' $ negate n
visitUnary (Minus', pos) _         = Left $ RuntimeError "Invalid operand" (show Minus', pos)
visitUnary (Bang, _) right         = Right $ Bool' $ not $ isTruthy right

visitBinary :: BinaryOp' -> Literal -> Literal -> Either RuntimeError Literal
visitBinary op left right
  | EqualEqual <- fst op = Right $ Bool' $ left == right
  | BangEqual <- fst op = Right $ Bool' $ left /= right
  | Number' l <- left
  , Number' r <- right = Right $ case fst op of
      Minus        -> Number' $ l - r
      Slash        -> Number' $ l / r
      Star         -> Number' $ l * r
      Plus         -> Number' $ l + r
      Greater      -> Bool' $ l > r
      GreaterEqual -> Bool' $ l >= r
      Less         -> Bool' $ l < r
      LessEqual    -> Bool' $ l <= r
  | String' l <- left
  , String' r <- right
  , Plus <- fst op =
      Right $ String' $ l ++ r
  | otherwise = Left $ RuntimeError "Invalid operands" $ first show op
