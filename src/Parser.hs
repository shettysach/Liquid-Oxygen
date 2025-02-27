module Parser where

import Ast as A
import Token as T

data ParseError = ParseError
  { message :: String
  , token :: Token
  }
  deriving (Show)

parse :: [Token] -> Either ParseError Expr
parse = fmap fst . expression

expression :: [Token] -> Either ParseError (Expr, [Token])
expression = equality

-- Binary

equality :: [Token] -> Either ParseError (Expr, [Token])
equality tokens = do
  (expr, rest) <- comparison tokens
  equality' expr rest
 where
  equality' :: Expr -> [Token] -> Either ParseError (Expr, [Token])
  equality' expr' [] = Right (expr', [])
  equality' expr' tokens'@(t : ts) =
    case fst t of
      T.BangEqual -> recurse A.BangEqual
      T.EqualEqual -> recurse A.EqualEqual
      _ -> Right (expr', tokens')
   where
    recurse op = do
      (right, rest') <- comparison ts
      equality' (Binary expr' op right) rest'

comparison :: [Token] -> Either ParseError (Expr, [Token])
comparison tokens = do
  (expr, rest) <- term tokens
  comparison' expr rest
 where
  comparison' :: Expr -> [Token] -> Either ParseError (Expr, [Token])
  comparison' expr' [] = Right (expr', [])
  comparison' expr' tokens'@(t : ts) =
    case fst t of
      T.Greater -> recurse A.Greater
      T.GreaterEqual -> recurse A.GreaterEqual
      T.Less -> recurse A.Less
      T.LessEqual -> recurse A.LessEqual
      _ -> Right (expr', tokens')
   where
    recurse op = do
      (right, rest') <- term ts
      comparison' (Binary expr' op right) rest'

term :: [Token] -> Either ParseError (Expr, [Token])
term tokens = do
  (expr, rest) <- factor tokens
  term' expr rest
 where
  term' :: Expr -> [Token] -> Either ParseError (Expr, [Token])
  term' expr' [] = Right (expr', [])
  term' expr' tokens'@(t : ts) =
    case fst t of
      T.Minus -> recurse A.Minus
      T.Plus -> recurse A.Plus
      _ -> Right (expr', tokens')
   where
    recurse op = do
      (right, rest') <- factor ts
      term' (Binary expr' op right) rest'

factor :: [Token] -> Either ParseError (Expr, [Token])
factor tokens = do
  (expr, rest) <- unary tokens
  factor' expr rest
 where
  factor' :: Expr -> [Token] -> Either ParseError (Expr, [Token])
  factor' expr' [] = Right (expr', [])
  factor' expr' tokens'@(t : ts) =
    case fst t of
      T.Slash -> recurse A.Slash
      T.Star -> recurse A.Star
      _ -> Right (expr', tokens')
   where
    recurse op = do
      (right, rest') <- unary ts
      factor' (Binary expr' op right) rest'

-- Unary

unary :: [Token] -> Either ParseError (Expr, [Token])
unary [] = primary []
unary tokens@(t : ts) =
  case fst t of
    T.Bang -> recurse A.Bang
    T.Minus -> recurse A.Minus'
    _ -> primary tokens
 where
  recurse op = do
    (right, rest') <- unary ts
    Right (Unary op right, rest')

-- Primary

primary :: [Token] -> Either ParseError (Expr, [Token])
primary [] = Left $ ParseError "Unexpected" (T.Eof, 0)
primary (t : ts) = case fst t of
  T.False' -> Right (Literal (Bool' False), ts)
  T.True' -> Right (Literal (Bool' True), ts)
  T.Nil -> Right (Literal A.Nil, ts)
  T.Number' n -> Right (Literal (A.Number' n), ts)
  T.String' s -> Right (Literal (A.String' s), ts)
  T.LeftParen -> do
    (expr, rest) <- expression ts
    case rest of
      t' : ts' | fst t' == T.RightParen -> Right (Grouping expr, ts')
      _ -> Left (ParseError "Expected ')' after expr" t)
  _ -> Left $ ParseError "Expect expr" t
