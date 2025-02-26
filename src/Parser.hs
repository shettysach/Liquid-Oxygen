module Parser where

import Ast as A
import Token as T

data ParseError = String'
  deriving (Show)

-- Binary

equality :: [Token] -> (Expr, [Token])
equality tokens = do
  let (expr, rest) = comparison tokens
  equality' expr rest
 where
  equality' :: Expr -> [Token] -> (Expr, [Token])
  equality' expr' [] = (expr', [])
  equality' expr' tokens@(t : ts) =
    if fst t `elem` [T.BangEqual, T.EqualEqual]
      then do
        let op = case fst t of
              T.BangEqual -> A.BangEqual
              T.EqualEqual -> A.EqualEqual
        let (right, rest') = comparison ts
        equality' (Binary expr' op right) rest'
      else (expr', tokens)

comparison :: [Token] -> (Expr, [Token])
comparison tokens = do
  let (expr, rest) = term tokens
  comparison' expr rest
 where
  comparison' :: Expr -> [Token] -> (Expr, [Token])
  comparison' expr' [] = (expr', [])
  comparison' expr' tokens@(t : ts) =
    if fst t `elem` [T.Less, T.LessEqual, T.Greater, T.GreaterEqual]
      then do
        let op = case fst t of
              T.Greater -> A.Greater
              T.GreaterEqual -> A.GreaterEqual
              T.Less -> A.Less
              T.LessEqual -> A.LessEqual
        let (right, rest') = term ts
        comparison' (Binary expr' op right) rest'
      else (expr', tokens)

term :: [Token] -> (Expr, [Token])
term tokens = do
  let (expr, rest) = factor tokens
  term' expr rest
 where
  term' :: Expr -> [Token] -> (Expr, [Token])
  term' expr' [] = (expr', [])
  term' expr' tokens@(t : ts) =
    if fst t `elem` [T.Minus, T.Plus]
      then do
        let op = case fst t of
              T.Minus -> A.Minus
              T.Plus -> A.Plus
        let (right, rest') = factor ts
        term' (Binary expr' op right) rest'
      else (expr', tokens)

factor :: [Token] -> (Expr, [Token])
factor tokens = do
  let (expr, rest) = unary tokens
  factor' expr rest
 where
  factor' :: Expr -> [Token] -> (Expr, [Token])
  factor' expr' [] = (expr', [])
  factor' expr' tokens@(t : ts) =
    if fst t `elem` [T.Slash, T.Star]
      then do
        let op = case fst t of
              T.Slash -> A.Slash
              T.Star -> A.Star
        let (right, rest') = unary ts
        factor' (Binary expr' op right) rest'
      else (expr', tokens)

-- Unary - one precedence higher

unary :: [Token] -> (Expr, [Token])
unary [] = primary []
unary tokens@(t : ts) =
  if fst t `elem` [T.Bang, T.Minus]
    then do
      let (right, rest') = unary ts
      let op = case fst t of
            T.Bang -> A.Bang
            T.Minus -> A.Minus'
      (Unary op right, rest')
    else primary tokens

-- Primary - one precedence higher

primary :: [Token] -> (Expr, [Token])
primary [] = error "Unexpected end of input"
primary tokens@(t : ts) = case fst t of
  T.False' -> (Literal (Bool' False), ts)
  T.True' -> (Literal (Bool' True), ts)
  T.Nil -> (Literal A.Nil, ts)
  T.Number' n -> (Literal (A.Number' n), ts)
  T.String' s -> (Literal (A.String' s), ts)
  T.LeftParen -> do
    let (expr, rest) = expression ts
    case rest of
      [] -> error "Expect ')' after expression"
      (t' : ts') ->
        if fst t' == T.RightParen
          then (Grouping expr, ts')
          else error "Expect ')' after expression"
  _ -> error $ "Unexpected token: " ++ show t

expression :: [Token] -> (Expr, [Token])
expression ts = undefined
