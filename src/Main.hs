module Main where

import Scanner

main :: IO ()
main = do
  input <- readFile "source.lox"
  let tokens = scan input
  putStrLn input
  putStrLn $ unlines (map show tokens)
