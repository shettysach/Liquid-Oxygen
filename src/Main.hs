import Interpreter (evaluate)
import Parser (parse)
import Scanner (scan)

main :: IO ()
main = do
  input <- readFile "source.lox"
  putStrLn input

  putStrLn "SCAN"
  case scan input of
    Left err -> print err
    Right tokens -> do
      putStrLn $ unlines (map show tokens)
      putStrLn "PARSE"
      case parse tokens of
        Left err -> print err
        Right expr -> do
          print expr
          putStrLn "\nEVALUATE"
          case evaluate expr of
            Left err -> print err
            Right lit -> do
              print lit
