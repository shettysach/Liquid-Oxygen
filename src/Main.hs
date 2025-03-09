import Interpreter        (interpret)
import Parser             (parse)
import Scanner            (scan)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      source <- readFile filename
      case scan source of
        Left err -> print err
        Right tokens -> case parse tokens of
          Left err         -> print err
          Right statements -> interpret statements >>= either print return
    _ -> putStrLn "Usage: lox <script>"
