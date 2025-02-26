module Parser where

import Ast as A
import Token as T

data ParseError = ParseError
  { message :: String
  , token :: Token
  }
  deriving (Show)

-- Binary

equality :: [Token] -> Either ParseError (Expr, [Token])
equality tokens = do
  (expr, rest) <- comparison tokens
  equality' expr rest
 where
  equality' :: Expr -> [Token] -> Either ParseError (Expr, [Token])
  equality' expr' [] = Right (expr', [])
  equality' expr' tokens@(t : ts) =
    if fst t `elem` [T.BangEqual, T.EqualEqual]
      then do
        (right, rest') <- comparison ts
        let op = case fst t of
              T.BangEqual -> A.BangEqual
              T.EqualEqual -> A.EqualEqual
        equality' (Binary expr' op right) rest'
      else Right (expr', tokens)

comparison :: [Token] -> Either ParseError (Expr, [Token])
comparison tokens = do
  (expr, rest) <- term tokens
  comparison' expr rest
 where
  comparison' :: Expr -> [Token] -> Either ParseError (Expr, [Token])
  comparison' expr' [] = Right (expr', [])
  comparison' expr' tokens@(t : ts) =
    if fst t `elem` [T.Less, T.LessEqual, T.Greater, T.GreaterEqual]
      then do
        (right, rest') <- term ts
        let op = case fst t of
              T.Greater -> A.Greater
              T.GreaterEqual -> A.GreaterEqual
              T.Less -> A.Less
              T.LessEqual -> A.LessEqual
        comparison' (Binary expr' op right) rest'
      else Right (expr', tokens)

term :: [Token] -> Either ParseError (Expr, [Token])
term tokens = do
  (expr, rest) <- factor tokens
  term' expr rest
 where
  term' :: Expr -> [Token] -> Either ParseError (Expr, [Token])
  term' expr' [] = Right (expr', [])
  term' expr' tokens@(t : ts) =
    if fst t `elem` [T.Minus, T.Plus]
      then do
        (right, rest') <- factor ts
        let op = case fst t of
              T.Minus -> A.Minus
              T.Plus -> A.Plus
        term' (Binary expr' op right) rest'
      else Right (expr', tokens)

factor :: [Token] -> Either ParseError (Expr, [Token])
factor tokens = do
  (expr, rest) <- unary tokens
  factor' expr rest
 where
  factor' :: Expr -> [Token] -> Either ParseError (Expr, [Token])
  factor' expr' [] = Right (expr', [])
  factor' expr' tokens@(t : ts) =
    if fst t `elem` [T.Slash, T.Star]
      then do
        (right, rest') <- unary ts
        let op = case fst t of
              T.Slash -> A.Slash
              T.Star -> A.Star
        factor' (Binary expr' op right) rest'
      else Right (expr', tokens)

-- Unary

unary :: [Token] -> Either ParseError (Expr, [Token])
unary [] = primary []
unary tokens@(t : ts) =
  if fst t `elem` [T.Bang, T.Minus]
    then do
      (right, rest') <- unary ts
      let op = case fst t of
            T.Bang -> A.Bang
            T.Minus -> A.Minus'
      Right (Unary op right, rest')
    else primary tokens

-- Primary

primary :: [Token] -> Either ParseError (Expr, [Token])
primary [] = Left $ ParseError "Unexpected" (T.Eof, 0)
primary tokens@(t : ts) = case fst t of
  T.False' -> Right (Literal (Bool' False), ts)
  T.True' -> Right (Literal (Bool' True), ts)
  T.Nil -> Right (Literal A.Nil, ts)
  T.Number' n -> Right (Literal (A.Number' n), ts)
  T.String' s -> Right (Literal (A.String' s), ts)
  T.LeftParen -> do
    let (expr, rest) = expression ts
    case rest of
      t' : ts' | fst t' == T.RightParen -> Right (Grouping expr, ts')
      t -> Left (ParseError "Expected ')' after expr" (head t))
  _ -> Left $ ParseError "Unexpected" t

-- Expr

expression :: [Token] -> (Expr, [Token])
expression ts = undefined
