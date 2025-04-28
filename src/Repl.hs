{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}

module Repl where

import Control.Monad ((>=>))
import Data.Map      qualified as Map
import System.IO     (hFlush, hPrint, stderr, stdout)

import Environment   (Scope, global)
import Interpreter   (interpretRepl)
import Parser        (parse)
import Resolver      (resolveRepl)
import Scanner       (scan)
import Syntax        (Env)

repl :: IO ()
repl = global >>= repl' [Map.empty]

repl' :: [Scope] -> Env -> IO ()
repl' scopes env = do
  putStr "> "
  hFlush stdout
  getLine >>= \case
    [] -> repl' scopes env
    "quit" -> putStrLn "\nExiting REPL."
    line -> runRepl line scopes env >>= uncurry repl'

runRepl :: String -> [Scope] -> Env -> IO ([Scope], Env)
runRepl input scopes env =
  (chainIO scan . Just >=> chainIO parse) input >>= \case
    Nothing -> pure (scopes, env)
    Just stmts -> case resolveRepl stmts scopes of
      Left err -> hPrint stderr err >> pure (scopes, env)
      Right (stmts', dists', scopes') ->
        interpretRepl (stmts', dists') env >>= \case
          Left err -> hPrint stderr err >> pure (scopes, env)
          Right env' -> pure (scopes', env')

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
