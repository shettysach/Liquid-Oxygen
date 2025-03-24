{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)

import Interpreter        (interpret)
import Parser             (parse)
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
    _ -> putStrLn "Usage: lox <script>"

chainIO :: (Show err) => (a -> Either err ok) -> Maybe a -> IO (Maybe ok)
chainIO _ Nothing = return Nothing
chainIO f (Just x) =
  case f x of
    Left err -> print err >> return Nothing
    Right ok -> return (Just ok)

endIO :: (Show err) => (a -> IO (Either err ok)) -> Maybe a -> IO ()
endIO _ Nothing = return ()
endIO f (Just x) =
  f x >>= \case
    Left err -> print err
    Right _ -> return ()
