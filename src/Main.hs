import Interpreter (evaluate)
import Parser (parse)
import Scanner (scan)

main :: IO ()
main = do
  input <- readFile "source.lox"
  putStrLn input

  case scan input of
    Left err -> print err
    Right tokens -> do
      putStrLn "SCAN"
      putStrLn $ unlines (map show tokens)
      case parse tokens of
        Left err -> print err
        Right expr -> do
          putStrLn "PARSE"
          print expr
          putStrLn "\nEVALUATE"
          print (evaluate expr)
