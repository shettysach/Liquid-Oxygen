{-# LANGUAGE ImportQualifiedPost #-}
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
import Data.IORef                 (modifyIORef, newIORef, readIORef)
import Data.Map                   qualified as Map

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
    (superLit, env') <- evaluate super dists env
    super' <- case superLit of
      Class'{} -> pure $ Just superLit
      _        -> throwE $ RuntimeError "Superclass must be a class" name
    envS <- liftIO $ liftIO (child env) >>= initialize "super" superLit
    methods' <- liftIO $ mapMethods methods dists envS Map.empty
    let klass = Class' name super' methods'
    envC <- liftIO (initialize (fst name) klass env')
    interpretStmts stmts dists envC
  Class name Nothing methods -> do
    methods' <- liftIO $ mapMethods methods dists env Map.empty
    let klass = Class' name Nothing methods'
    envC <- liftIO $ initialize (fst name) klass env
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

type Eval = ExceptT RuntimeError (StateT Env IO)

evaluate :: Expr -> Distances -> Env -> ExceptT RuntimeError IO (Literal, Env)
evaluate expr dists env = ExceptT $ do
  (result, env') <- runStateT (runExceptT $ evalExpr expr dists) env
  pure $ result <&> (,env')

evalExpr :: Expr -> Distances -> Eval Literal
evalExpr expr dists = case expr of
  Literal lit -> pure lit
  Grouping env -> evalExpr env dists
  Variable var -> lift get >>= ExceptT . liftIO . getAt var dists
  Assignment var rhs -> do
    val <- evalExpr rhs dists
    dist <- except $ getDistance var dists
    lift get >>= liftIO . assignAt var val dist >> pure val
  Unary op right -> evalExpr right dists >>= visitUnary op
  Binary op left right -> do
    l <- evalExpr left dists
    r <- evalExpr right dists
    visitBinary op l r
  Logical op left right -> do
    l <- evalExpr left dists
    r <- evalExpr right dists
    visitLogical op l r
  Call (callee, pos) argExprs -> do
    calleeLit <- evalExpr callee dists
    argLits <- mapM (`evalExpr` dists) (reverse argExprs)
    case calleeLit of
      Function'{}    -> callFunction calleeLit argLits
      Class'{}       -> callClass calleeLit argLits
      NativeFn n f a -> callFunction (Function' (n, pos) f a undefined) argLits
      literal        -> throwE $ RuntimeError "Calling non-function/non-class" (show literal, pos)
  Get instExpr field -> do
    instance' <- evalExpr instExpr dists
    case instance' of
      Instance' klass fRef -> do
        fields <- liftIO $ readIORef fRef
        case Map.lookup (fst field) fields of
          Just property -> pure property
          Nothing       -> findMethod (Just klass) field instance'
      literal -> throwE $ RuntimeError "Only instances have properties/methods" (show literal, snd field)
  Set instExpr field rhs -> do
    inst <- evalExpr instExpr dists
    val <- evalExpr rhs dists
    case inst of
      Instance' _ fRef -> liftIO $ modifyIORef fRef $ Map.insert (fst field) val
      literal          -> throwE $ RuntimeError "Only instances have properties/methods" (show literal, snd field)
    pure val
  This pos -> lift get >>= ExceptT . liftIO . getAt ("this", pos) dists
  Super pos mthd -> do
    env <- lift get
    dist <- except $ getDistance ("super", pos) dists
    super <- ExceptT . liftIO $ getHere ("super", pos) $ ancestor env dist
    inst <- ExceptT . liftIO $ getHere ("this", pos) $ ancestor env (dist - 1)
    findMethod (Just super) mthd inst

findMethod :: Maybe Literal -> String' -> Literal -> Eval Literal
findMethod (Just (Class' _ super methods)) field inst =
  case Map.lookup (fst field) methods of
    Just mthd -> bindThis inst mthd
    Nothing   -> findMethod super field inst
findMethod Nothing field _ = throwE $ RuntimeError "Undefined field" field
findMethod _ _ _ = undefined

bindThis :: Literal -> Literal -> Eval Literal
bindThis instance' (Function' name func arity closure) = do
  closure' <- liftIO $ child closure >>= initialize "this" instance'
  pure $ Function' name func arity closure'
bindThis _ _ = undefined

callFunction :: Literal -> [Literal] -> Eval Literal
callFunction (Function' name func arity closure) args =
  if length args /= arity
    then throwE $ RuntimeError ("Arity /= " ++ show arity) name
    else ExceptT . fmap (fmap fst) . liftIO $ func args closure
callFunction _ _ = undefined

callClass :: Literal -> [Literal] -> Eval Literal
callClass klass@(Class' name super methods) args = do
  instance' <- liftIO $ newIORef Map.empty <&> Instance' klass
  case Map.lookup "init" methods of
    Just (Function' _ initr arity closure) -> initInstance initr name args arity closure instance'
    Nothing -> do
      result <- lift . runExceptT $ findMethod super ("init", snd name) instance'
      case result of
        Right (Function' _ initr arity closure) -> initInstance initr name args arity closure instance'
        Right _                                 -> throwE $ RuntimeError "Invalid init in superclass" name
        Left _ | null args                      -> pure instance'
        Left err                                -> throwE err
    _ -> throwE $ RuntimeError "Invalid init in class definition" name
callClass _ _ = undefined

initInstance :: Callable -> String' -> [Literal] -> Int -> Env -> Literal -> Eval Literal
initInstance initr name args arity closure instance' =
  if length args /= arity
    then throwE $ RuntimeError ("Arity /= " ++ show arity) name
    else liftIO $ child closure >>= initialize "this" instance' >>= liftIO . initr args >> pure instance'

visitLogical :: LogicalOp' -> Literal -> Literal -> Eval Literal
visitLogical op left right = case fst op of
  Or | isTruthy left        -> pure left
  And | not $ isTruthy left -> pure left
  _                         -> pure right

visitUnary :: UnaryOp' -> Literal -> Eval Literal
visitUnary (Minus', _) (Number' n) = pure $ Number' $ negate n
visitUnary (Minus', pos) _         = throwE $ RuntimeError "Invalid operand" (show Minus', pos)
visitUnary (Bang, _) right         = pure $ Bool' $ not $ isTruthy right

visitBinary :: BinaryOp' -> Literal -> Literal -> Eval Literal
visitBinary (EqualEqual, _) left right = pure $ Bool' $ left == right
visitBinary (BangEqual, _) left right = pure $ Bool' $ left /= right
visitBinary (op, _) (Number' l) (Number' r) = case op of
  Minus        -> pure $ Number' $ l - r
  Slash        -> pure $ Number' $ l / r
  Star         -> pure $ Number' $ l * r
  Plus         -> pure $ Number' $ l + r
  Greater      -> pure $ Bool' $ l > r
  GreaterEqual -> pure $ Bool' $ l >= r
  Less         -> pure $ Bool' $ l < r
  LessEqual    -> pure $ Bool' $ l <= r
visitBinary (Plus, _) (String' l) (String' r) = pure $ String' $ l ++ r
visitBinary (op, pos) _ _ = throwE $ RuntimeError "Invalid operands" (show op, pos)
