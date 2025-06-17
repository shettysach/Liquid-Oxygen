{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)
import System.IO          (hPrint, stderr)

import Environment        (global, start)
import Interpreter        (interpret)
import Parser             (parse)
import Repl               (repl)
import Resolver           (resolve)
import Scanner            (scan)

main :: IO ()
main =
  getArgs >>= \case
    [file] -> do
      source <- readFile file
      tokens <- chainIO scan $ Just source
      stmts <- chainIO parse tokens
      dists <- chainIO resolve stmts
      endIO interpret stmts dists
    _ -> global >>= repl start

chainIO :: (Show l) => (x -> Either l r) -> Maybe x -> IO (Maybe r)
chainIO f mx = case traverse f mx of
  Left l  -> hPrint stderr l >> pure Nothing
  Right r -> pure r

endIO :: (Show l) => (x -> y -> IO (Either l ())) -> Maybe x -> Maybe y -> IO ()
endIO f (Just x) (Just y) =
  f x y >>= \case
    Left l -> hPrint stderr l
    Right r -> pure r
endIO _ _ _ = pure ()
