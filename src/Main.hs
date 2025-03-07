{-# LANGUAGE LambdaCase #-}

import Interpreter (interpret)
import Parser      (parse)
import Scanner     (scan)

main :: IO ()
main = do
  readFile "source.lox"
    >>= \source ->
      chainIO scan (Just source)
        >>= chainIO parse
        >>= endIO interpret

chainIO :: (Show err) => (a -> Either err ok) -> Maybe a -> IO (Maybe ok)
chainIO _ Nothing = return Nothing
chainIO f (Just x) = case f x of
  Left err -> print err >> return Nothing
  Right ok -> return (Just ok)

endIO :: (Show err) => (a -> IO (Either err ok)) -> Maybe a -> IO ()
endIO _ Nothing = return ()
endIO f (Just x) =
  f x >>= \case
    Left err -> print err
    Right _ -> return ()
