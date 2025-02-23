module Main where

import Scanner

main :: IO ()
main = do
  input <- readFile "source.lox"

  putStrLn input
  case scan input of
    Left err -> putStrLn ("Error: " ++ err)
    Right tokens -> do
      putStrLn $ unlines (map show tokens)
