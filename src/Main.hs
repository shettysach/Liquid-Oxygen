import Interpreter (interpret)
import Parser      (parse)
import Scanner     (scan)

main :: IO ()
main =
  readFile "source.lox"
    >>= \source ->
      chainIO scan (Just source)
        >>= chainIO parse
        >>= endIO interpret

chainIO ::
  (Show err, Show res) => (a -> Either err res) -> Maybe a -> IO (Maybe res)
chainIO _ Nothing = return Nothing
chainIO f (Just x) = case f x of
  Left err -> print err >> return Nothing
  Right ok -> return (Just ok)

endIO ::
  (Show err, Show res) => (a -> IO (Either err res)) -> Maybe a -> IO ()
endIO _ Nothing = return ()
endIO f (Just x) = do
  result <- f x
  case result of
    Left err -> print err
    Right _  -> return ()
