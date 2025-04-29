{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}

module Repl where

import Data.Map    qualified as Map
import System.IO   (hFlush, hPrint, stderr, stdout)

import Environment (Scope, global)
import Error       (ScanError)
import Interpreter (interpretRepl)
import Parser      (parse)
import Resolver    (resolveRepl)
import Scanner     (scan)
import Syntax      (Env)
import Token       as T

repl :: IO ()
repl = do
  putStrLn "Welcome to the REPL. Type `quit` to quit."
  global >>= repl' [Map.empty]

repl' :: [Scope] -> Env -> IO ()
repl' scopes env = do
  putStr "> "
  hFlush stdout
  readMultiline >>= \case
    Right "quit" -> putStrLn "\nExiting REPL."
    Right [] -> repl' scopes env
    Right line -> runRepl line scopes env >>= uncurry repl'
    Left err -> hPrint stderr err >> repl' scopes env

runRepl :: String -> [Scope] -> Env -> IO ([Scope], Env)
runRepl input scopes env =
  chainIO scan (Just input) >>= chainIO parse >>= \case
    Nothing -> pure (scopes, env)
    Just stmts -> case resolveRepl stmts scopes of
      Left err -> hPrint stderr err >> pure (scopes, env)
      Right (stmts', dists', scopes') ->
        interpretRepl (stmts', dists') env >>= \case
          Left err -> hPrint stderr err >> pure (scopes, env)
          Right env' -> pure (scopes', env')

--

chainIO :: (Show l) => (a -> Either l r) -> Maybe a -> IO (Maybe r)
chainIO _ Nothing = pure Nothing
chainIO f (Just x) =
  case f x of
    Left l  -> hPrint stderr l >> pure Nothing
    Right r -> pure $ pure r

endIO :: (Show l) => (a -> IO (Either l ())) -> Maybe a -> IO ()
endIO _ Nothing = return ()
endIO f (Just x) =
  f x >>= \case
    Left l -> hPrint stderr l
    Right _ -> pure ()

--

readMultiline :: IO (Either ScanError String)
readMultiline = do
  hFlush stdout
  line <- getLine
  case scan line of
    Left err -> pure $ Left err
    Right tokens ->
      if braceCount tokens > 0
        then readBlock [line] (braceCount tokens)
        else pure $ Right line

readBlock :: [String] -> Int -> IO (Either ScanError String)
readBlock acc 0 = pure $ Right $ unlines $ reverse acc
readBlock acc n = do
  putStr "| "
  hFlush stdout
  next <- getLine
  case scan next of
    Right ts -> readBlock (next : acc) (n + braceCount ts)
    Left err -> pure $ Left err

braceCount :: [Token] -> Int
braceCount = foldr count 0
 where
  count (T.LeftBrace, _) acc  = acc + 1
  count (T.RightBrace, _) acc = acc - 1
  count _ acc                 = acc
