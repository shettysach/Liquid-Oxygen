{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE TupleSections       #-}

module Interpreter where

import Control.Arrow              (second)
import Control.Monad              (void)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT, throwE)
import Control.Monad.Trans.State  (StateT (runStateT), get)
import Data.Foldable              (foldrM)
import Data.Functor               ((<&>))
import Data.HashMap.Strict        qualified as HashMap
import Data.IORef                 (modifyIORef, newIORef, readIORef)

import Environment
import Error                      (RuntimeError (RuntimeError))
import Position                   (lengthW)
import Syntax

interpretFile :: [Stmt] -> Distances -> IO (Either RuntimeError ())
interpretFile stmts dists = global >>= runExceptT . interpretStmts stmts dists <&> void

interpretRepl :: [Stmt] -> Distances -> Env -> IO (Either RuntimeError Env)
interpretRepl stmts dists = fmap (fmap snd) . runExceptT . interpretStmts stmts dists

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
    liftIO (child env) >>= interpretStmts stmts' dists >>= \case
      (Nil, env') -> interpretStmts stmts dists $ parent env'
      (lit, env') -> pure (lit, env')
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
  Function fn -> mdo
    closure <- lift $ initialize name func env
    (name, func) <- lift $ interpretFunction fn dists closure
    interpretStmts stmts dists closure
  Class name (Just super) methods -> do
    (superLit, env') <- evaluate super dists env
    super' <- case superLit of
      Class' super' -> pure $ Just super'
      _             -> throwE $ RuntimeError "Superclass must be a class" name
    envS <- liftIO $ liftIO (child env) >>= initialize "super" superLit
    methods' <- liftIO $ mapMethods methods dists envS HashMap.empty
    let klass = ClsLit name super' methods'
    envC <- liftIO $ initialize (fst name) (Class' klass) env'
    interpretStmts stmts dists envC
  Class name Nothing methods -> do
    methods' <- liftIO $ mapMethods methods dists env HashMap.empty
    let klass = ClsLit name Nothing methods'
    envC <- liftIO $ initialize (fst name) (Class' klass) env
    interpretStmts stmts dists envC

interpretFunction :: FnStmt -> Distances -> Env -> IO (String, Literal)
interpretFunction ((FnStmt name params body)) dists closure =
  let callable args env =
        child env
          >>= foldrM (uncurry initialize) `flip` zip (map fst params) args
          >>= runExceptT . interpretStmts body dists
      func = FnLit name callable (lengthW params) closure
   in pure (fst name, Function' func)

mapMethods :: [FnStmt] -> Distances -> Env -> HashMap.HashMap String FnLit -> IO (HashMap.HashMap String FnLit)
mapMethods [] _ _ mthds = pure mthds
mapMethods (m : ms) dists closure mthds = do
  (name, Function' func) <- interpretFunction m dists closure
  mapMethods ms dists closure $ HashMap.insert name func mthds

evaluate :: Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
evaluate expr dists env = ExceptT $ do
  (result, state) <- runStateT (runExceptT $ evalExpr expr) (dists, env)
  pure $ result <&> (,snd state)

type EnvState = ExceptT RuntimeError (StateT (Distances, Env) IO)

evalExpr :: Expr -> EnvState Literal
evalExpr expr = case expr of
  Literal lit -> pure lit
  Grouping expr' -> evalExpr expr'
  Variable var -> lift get >>= ExceptT . liftIO . uncurry (getAt var)
  Assignment var rhs -> do
    val <- evalExpr rhs
    (dists, env) <- lift get
    dist <- except $ getDistance var dists
    liftIO $ assignAt var val dist env >> pure val
  Unary op right -> evalExpr right >>= evalUnary op
  Binary op left right -> do
    l <- evalExpr left
    r <- evalExpr right
    evalBinary op l r
  Logical op left right -> do
    l <- evalExpr left
    r <- evalExpr right
    evalLogical op l r
  Call (callee, pos) argExprs -> do
    calleeLit <- evalExpr callee
    argLits <- mapM evalExpr (reverse argExprs)
    case calleeLit of
      Function' func           -> callFunction func argLits
      NativeFn name func arity -> callFunction (FnLit (name, pos) func arity undefined) argLits
      Class' klass             -> callClass klass argLits
      literal                  -> throwE $ RuntimeError "Calling non-function/non-class" (show literal, pos)
  Get instExpr field -> do
    instance' <- evalExpr instExpr
    case instance' of
      Instance' klass fRef -> do
        fields <- liftIO $ readIORef fRef
        case HashMap.lookup (fst field) fields of
          Just property -> pure property
          Nothing       -> findMethod (Just klass) field instance' <&> Function'
      literal -> throwE $ RuntimeError "Only instances have properties/methods" (show literal, snd field)
  Set instExpr field rhs -> do
    inst <- evalExpr instExpr
    val <- evalExpr rhs
    case inst of
      Instance' _ fRef -> liftIO $ modifyIORef fRef $ HashMap.insert (fst field) val
      literal          -> throwE $ RuntimeError "Only instances have properties/methods" (show literal, snd field)
    pure val
  This pos -> lift get >>= ExceptT . liftIO . uncurry (getAt ("this", pos))
  Super pos method -> do
    (dists, env) <- lift get
    dist <- except $ getDistance ("super", pos) dists
    Class' super <- ExceptT . liftIO . getHere ("super", pos) $ ancestor env dist
    instance' <- ExceptT . liftIO . getHere ("this", pos) $ ancestor env (dist - 1)
    findMethod (Just super) method instance' <&> Function'

callFunction :: FnLit -> [Literal] -> EnvState Literal
callFunction (FnLit name func arity closure) args =
  if lengthW args == arity
    then ExceptT . (fmap . fmap) fst . liftIO $ func args closure
    else throwE $ RuntimeError ("Arity /= " ++ show arity) name

callClass :: ClsLit -> [Literal] -> EnvState Literal
callClass klass@(ClsLit name super methods) args = do
  instance' <- liftIO $ newIORef HashMap.empty <&> Instance' klass
  case HashMap.lookup "init" methods of
    Just func -> initInstance func name args instance'
    Nothing ->
      lift (runExceptT $ findMethod super ("init", snd name) instance') >>= \case
        Right func -> initInstance func name args instance'
        Left _ | null args -> pure instance'
        Left err -> throwE err

initInstance :: FnLit -> String' -> [Literal] -> Literal -> EnvState Literal
initInstance (FnLit _ initr arity closure) name args instance' =
  if lengthW args /= arity
    then throwE $ RuntimeError ("Arity /= " ++ show arity) name
    else liftIO $ child closure >>= initialize "this" instance' >>= liftIO . initr args >> pure instance'

evalLogical :: LogicalOp' -> Literal -> Literal -> EnvState Literal
evalLogical op left right = case fst op of
  Or | isTruthy left        -> pure left
  And | not $ isTruthy left -> pure left
  _                         -> pure right

evalUnary :: UnaryOp' -> Literal -> EnvState Literal
evalUnary (Minus', _) (Number' n) = pure $ Number' $ negate n
evalUnary (Bang, _) right         = pure $ Bool' $ not $ isTruthy right
evalUnary (Minus', pos) _         = throwE $ RuntimeError "Invalid operand" (show Minus', pos)

evalBinary :: BinaryOp' -> Literal -> Literal -> EnvState Literal
evalBinary (EqualEqual, _) left right = pure $ Bool' $ left == right
evalBinary (BangEqual, _) left right = pure $ Bool' $ left /= right
evalBinary (op, _) (Number' l) (Number' r) = pure $ case op of
  Minus        -> Number' $ l - r
  Slash        -> Number' $ l / r
  Star         -> Number' $ l * r
  Plus         -> Number' $ l + r
  Greater      -> Bool' $ l > r
  GreaterEqual -> Bool' $ l >= r
  Less         -> Bool' $ l < r
  LessEqual    -> Bool' $ l <= r
evalBinary (Plus, _) (String' l) (String' r) = pure $ String' $ l ++ r
evalBinary (op, pos) _ _ = throwE $ RuntimeError "Invalid operands" (show op, pos)

findMethod :: Maybe ClsLit -> String' -> Literal -> EnvState FnLit
findMethod (Just (ClsLit _ super methods)) field inst =
  case HashMap.lookup (fst field) methods of
    Just mthd -> bindThis mthd inst
    Nothing   -> findMethod super field inst
findMethod Nothing field _ = throwE $ RuntimeError "Undefined field" field

bindThis :: FnLit -> Literal -> EnvState FnLit
bindThis (FnLit name func arity closure) instance' = liftIO $ child closure >>= initialize "this" instance' <&> FnLit name func arity
