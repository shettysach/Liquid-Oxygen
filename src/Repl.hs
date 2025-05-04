{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}

module Repl where

import Control.Monad.Trans.Except (runExceptT)
import Data.Map                   qualified as Map
import System.IO                  (hFlush, hPrint, stderr, stdout)

import Data.List.NonEmpty         (NonEmpty)
import Environment                (Scope, global)
import Error                      (ScanError)
import Interpreter                (evaluate, replInterpret)
import Parser                     (expression, parse)
import Resolver                   (replResolve)
import Scanner                    (scan)
import Syntax                     as S
import Token                      as T

startRepl :: IO ()
startRepl = do
  putStrLn "Welcome to the REPL. Type `quit` to quit."
  global >>= repl [Map.empty]
  putStrLn "Exiting the REPL."

repl :: [Scope] -> Env -> IO ()
repl scopes env = do
  putStr "-> "
  hFlush stdout
  readMultiline >>= \case
    Right "quit" -> pure ()
    Right [] -> repl scopes env
    Right line -> runInput line scopes env >>= uncurry repl
    Left err -> hPrint stderr err >> repl scopes env

runInput :: String -> [Scope] -> Env -> IO ([Scope], Env)
runInput input scopes env = case scan input of
  Left err -> hPrint stderr err >> pure (scopes, env)
  Right tokens -> case parse tokens of
    Right stmts -> runStmts stmts scopes env
    Left err -> case fst <$> expression tokens of
      Left _     -> hPrint stderr err >> pure (scopes, env)
      Right expr -> runExpr expr scopes env

runStmts :: [Stmt] -> [Scope] -> Env -> IO ([Scope], Env)
runStmts stmts scopes env = case replResolve stmts scopes of
  Left err -> hPrint stderr err >> pure (scopes, env)
  Right (stmts', dists', scopes') ->
    replInterpret (stmts', dists') env >>= \case
      Left err -> hPrint stderr err >> pure (scopes, env)
      Right env' -> pure (scopes', env')

runExpr :: Expr -> [Scope] -> Env -> IO ([Scope], Env)
runExpr expr scopes env = case replResolve [S.Expr expr] scopes of
  Left err -> hPrint stderr err >> pure (scopes, env)
  Right ([S.Expr expr'], dists', scopes') ->
    runExceptT (evaluate expr' dists' env) >>= \case
      Left err -> hPrint stderr err >> pure (scopes, env)
      Right (val, env') -> putStrLn ("<- " ++ show val) >> pure (scopes', env')
  _ -> undefined

readMultiline :: IO (Either ScanError String)
readMultiline = do
  hFlush stdout
  line <- getLine
  case scan line of
    Left err -> pure $ Left err
    Right tokens ->
      if braceCount tokens > 0
        then readBlock [line] $ braceCount tokens
        else pure $ Right line

readBlock :: [String] -> Int -> IO (Either ScanError String)
readBlock acc 0 = pure $ Right $ unlines $ reverse acc
readBlock acc n = do
  putStr " ∙ "
  hFlush stdout
  next <- getLine
  case scan next of
    Right ts -> readBlock (next : acc) (n + braceCount ts)
    Left err -> pure $ Left err

braceCount :: NonEmpty Token -> Int
braceCount = foldr count 0
 where
  count (T.LeftBrace, _) acc  = acc + 1
  count (T.RightBrace, _) acc = acc - 1
  count _ acc                 = acc
