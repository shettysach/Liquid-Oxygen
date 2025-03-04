module Parser where

import Ast   as A
import Token as T

data ParseError = ParseError
  { message :: String
  , token   :: Token
  }
  deriving (Show)

parse :: [Token] -> Either ParseError [Stmt]
parse tokens = parse' (init tokens) []
 where
  parse' :: [Token] -> [Stmt] -> Either ParseError [Stmt]
  parse' [] stmts = Right (reverse stmts)
  parse' tokens' stmts = do
    (stmt, rest) <- statement tokens'
    parse' rest (stmt : stmts)

statement :: [Token] -> Either ParseError (Stmt, [Token])
statement [] = Left $ ParseError "Unexpected" (T.Eof, (0, 0))
statement tokens@(t : ts) =
  case fst t of
    T.Print -> statement' ts A.Print
    _       -> statement' tokens A.Expr
 where
  statement' tokens' stmtType = do
    (expr, ts') <- expression tokens'
    case ts' of
      ((T.Semicolon, _) : ts'') -> Right (stmtType expr, ts'')
      (t' : _) -> Left $ ParseError "Expect ';' after value." t'
      [] -> Left $ ParseError "Unexpected 1" (T.Eof, (0, 0))

expression :: [Token] -> Either ParseError (Expr, [Token])
expression = equality

-- Binary

equality :: [Token] -> Either ParseError (Expr, [Token])
equality tokens = do
  comparison tokens >>= uncurry equality'
 where
  equality' expr [] = Right (expr, [])
  equality' expr tokens'@(t : ts) =
    case fst t of
      T.BangEqual  -> recurse A.BangEqual
      T.EqualEqual -> recurse A.EqualEqual
      _            -> Right (expr, tokens')
   where
    recurse op = do
      (right, ts') <- comparison ts
      equality' (Binary op expr right) ts'

comparison :: [Token] -> Either ParseError (Expr, [Token])
comparison tokens = do
  term tokens >>= uncurry comparison'
 where
  comparison' expr [] = Right (expr, [])
  comparison' expr tokens'@(t : ts) =
    case fst t of
      T.Greater      -> recurse A.Greater
      T.GreaterEqual -> recurse A.GreaterEqual
      T.Less         -> recurse A.Less
      T.LessEqual    -> recurse A.LessEqual
      _              -> Right (expr, tokens')
   where
    recurse op = do
      (right, ts') <- term ts
      comparison' (Binary op expr right) ts'

term :: [Token] -> Either ParseError (Expr, [Token])
term tokens = do
  factor tokens >>= uncurry term'
 where
  term' expr [] = Right (expr, [])
  term' expr tokens'@(t : ts) =
    case fst t of
      T.Minus -> recurse A.Minus
      T.Plus  -> recurse A.Plus
      _       -> Right (expr, tokens')
   where
    recurse op = do
      (right, ts') <- factor ts
      term' (Binary op expr right) ts'

factor :: [Token] -> Either ParseError (Expr, [Token])
factor tokens = do
  unary tokens >>= uncurry factor'
 where
  factor' expr [] = Right (expr, [])
  factor' expr tokens'@(t : ts) =
    case fst t of
      T.Slash -> recurse A.Slash
      T.Star  -> recurse A.Star
      _       -> Right (expr, tokens')
   where
    recurse op = do
      (right, ts') <- unary ts
      factor' (Binary op expr right) ts'

-- Unary

unary :: [Token] -> Either ParseError (Expr, [Token])
unary [] = primary []
unary tokens@(t : ts) =
  case fst t of
    T.Bang  -> recurse A.Bang
    T.Minus -> recurse A.Minus'
    _       -> primary tokens
 where
  recurse op = do
    (right, ts') <- unary ts
    Right (Unary op right, ts')

-- Primary

primary :: [Token] -> Either ParseError (Expr, [Token])
primary [] = Left $ ParseError "Unexpected" (T.Eof, (0, 0))
primary (t : ts) = case fst t of
  T.False' -> Right (Literal (Bool' False), ts)
  T.True' -> Right (Literal (Bool' True), ts)
  T.Nil -> Right (Literal A.Nil, ts)
  T.Number' n -> Right (Literal (A.Number' n), ts)
  T.String' s -> Right (Literal (A.String' s), ts)
  T.LeftParen -> do
    (expr, ts') <- expression ts
    case ts' of
      (T.RightParen, _) : ts'' -> Right (Grouping expr, ts'')
      _                        -> Left $ ParseError "Expected ')' after expr" t
  _ -> Left $ ParseError "Expected expr" t
