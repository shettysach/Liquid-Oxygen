{-# LANGUAGE LambdaCase #-}

import Control.Monad      (forM_)
import System.Environment (getArgs)
import System.IO          (hPrint, stderr)

import Environment        (global, start)
import Interpreter        (interpretFile)
import Parser             (parse)
import Repl               (repl)
import Resolver           (resolveFile)
import Scanner            (scan)

main :: IO ()
main =
  getArgs >>= \case
    [] -> global >>= repl start
    files -> forM_ files $ \file -> do
      source <- readFile file
      tokens <- chainIO scan $ Just source
      stmts <- chainIO parse tokens
      dists <- chainIO resolveFile stmts
      endIO interpretFile stmts dists

chainIO :: (Show l) => (x -> Either l r) -> Maybe x -> IO (Maybe r)
chainIO f mx = case traverse f mx of
  Left l  -> hPrint stderr l >> pure Nothing
  Right r -> pure r

endIO :: (Show l) => (x -> y -> IO (Either l ())) -> Maybe x -> Maybe y -> IO ()
endIO f mx my =
  sequenceA (liftA2 f mx my) >>= \case
    Just (Left l) -> hPrint stderr l
    _ -> pure ()
