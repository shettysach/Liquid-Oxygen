{-# LANGUAGE LambdaCase #-}

import Control.Arrow      ((&&&))
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
    [file] ->
      readFile file
        >>= chainIO scan . Just
        >>= chainIO parse
        >>= sequenceA . (id &&& chainIO resolve)
        >>= endIO interpret
    _ -> global >>= repl start

chainIO :: (Show l) => (a -> Either l r) -> Maybe a -> IO (Maybe r)
chainIO f (Just x) = case f x of
  Left l  -> hPrint stderr l >> pure Nothing
  Right r -> pure $ Just r
chainIO _ Nothing = pure Nothing

endIO :: (Show l) => (a -> b -> IO (Either l ())) -> (Maybe a, Maybe b) -> IO ()
endIO f (Just x, Just y) =
  f x y >>= \case
    Left l -> hPrint stderr l
    Right _ -> pure ()
endIO _ _ = pure ()
