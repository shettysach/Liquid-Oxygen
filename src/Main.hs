{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)

import Interpreter        (interpret)
import Parser             (parse)
import Resolver           (resolve)
import Scanner            (scan)

-- main :: IO ()
-- main =
--   getArgs >>= \case
--     [file] ->
--       readFile file
--         >>= chainIO scan . Just
--         >>= chainIO parse
--         >>= chainIO resolve
--         >>= endIO interpret
--     _ -> putStrLn "Usage: lox <script>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      content <- readFile file
      tokens <- chainIO scan (Just content)
      stmts <- chainIO parse tokens
      dists <- chainIO resolve stmts
      endIO interpret dists
    _ -> putStrLn "Usage: lox <script>"

chainIO :: (Show l) => (a -> Either l r) -> Maybe a -> IO (Maybe r)
chainIO _ Nothing = return Nothing
chainIO f (Just x) =
  case f x of
    Left l  -> print l >> return Nothing
    Right r -> return (Just r)

endIO :: (Show l) => (a -> IO (Either l r)) -> Maybe a -> IO ()
endIO _ Nothing = return ()
endIO f (Just x) =
  f x >>= \case
    Left err -> print err
    Right _ -> return ()
