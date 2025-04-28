{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)

import Interpreter        (interpret)
import Parser             (parse)
import Repl               (chainIO, endIO, repl)
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
    _ -> repl
