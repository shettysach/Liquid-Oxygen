{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)
import System.IO          (hPrint, stderr)

import Interpreter        (interpret)
import Parser             (parse)
import Repl               (startRepl)
import Resolver           (resolve)
import Scanner            (scan)

main :: IO ()
main =
  getArgs >>= \case
    [file] ->
      readFile file
        >>= chainIO scan . Just
        >>= chainIO parse
        >>= chainIO resolve
        >>= endIO interpret
    _ -> startRepl

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
