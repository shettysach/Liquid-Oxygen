import Scanner (scan)

main :: IO ()
main = do
  input <- readFile "source.lox"
  putStrLn input

  case scan input of
    Left err -> print err
    Right tokens -> putStrLn $ unlines (map show tokens)
