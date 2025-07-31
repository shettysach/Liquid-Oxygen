{-# LANGUAGE LambdaCase #-}

module Repl where

import Control.Monad.Trans.Except (runExceptT)
import Data.List.NonEmpty         (NonEmpty)
import System.IO                  (hFlush, hPrint, stderr, stdout)

import Environment                (Scope)
import Error                      (ScanError)
import Interpreter                (evaluate, replInterpret)
import Parser                     (expression, parse)
import Resolver                   (replResolve)
import Scanner                    (scan)
import Syntax                     (Env, Expr, Stmt (Expr))
import Token                      (Token, TokenType (LeftBrace, RightBrace))

repl :: NonEmpty Scope -> Env -> IO ()
repl scopes env = do
  putStr "> "
  hFlush stdout
  readMultiline >>= \case
    Right [] -> pure ()
    Right line -> runInput line scopes env >>= uncurry repl
    Left err -> hPrint stderr err >> repl scopes env

runInput :: String -> NonEmpty Scope -> Env -> IO (NonEmpty Scope, Env)
runInput input scopes env = case scan input of
  Left err -> hPrint stderr err >> pure (scopes, env)
  Right tokens -> case parse tokens of
    Right stmts -> runStmts stmts scopes env
    Left err -> case fst <$> expression tokens of
      Left _     -> hPrint stderr err >> pure (scopes, env)
      Right expr -> runExpr expr scopes env

runStmts :: [Stmt] -> NonEmpty Scope -> Env -> IO (NonEmpty Scope, Env)
runStmts stmts scopes env = case replResolve stmts scopes of
  Left err -> hPrint stderr err >> pure (scopes, env)
  Right (dists', scopes') ->
    replInterpret stmts dists' env >>= \case
      Left err -> hPrint stderr err >> pure (scopes, env)
      Right env' -> pure (scopes', env')

runExpr :: Expr -> NonEmpty Scope -> Env -> IO (NonEmpty Scope, Env)
runExpr expr scopes env = case replResolve [Expr expr] scopes of
  Left err -> hPrint stderr err >> pure (scopes, env)
  Right (dists', scopes') ->
    runExceptT (evaluate expr dists' env) >>= \case
      Left err -> hPrint stderr err >> pure (scopes, env)
      Right (lit, env') -> print lit >> pure (scopes', env')

readMultiline :: IO (Either ScanError String)
readMultiline = do
  hFlush stdout
  line <- getLine
  case scan line of
    Left err -> pure $ Left err
    Right tokens ->
      if braceCount tokens == 0
        then pure $ Right line
        else readBlock [line] $ braceCount tokens

readBlock :: [String] -> Int -> IO (Either ScanError String)
readBlock acc 0 = pure $ Right $ unlines $ reverse acc
readBlock acc n = do
  putStr "∙ "
  hFlush stdout
  next <- getLine
  case scan next of
    Right ts -> readBlock (next : acc) (n + braceCount ts)
    Left err -> pure $ Left err

braceCount :: NonEmpty Token -> Int
braceCount = sum . fmap delta
 where
  delta (LeftBrace, _)  = 1
  delta (RightBrace, _) = -1
  delta _               = 0
