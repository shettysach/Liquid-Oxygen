module Parser where

import Ast           as A
import Control.Arrow (Arrow (first))
import Token         as T

data ParseError = ParseError
  { message :: String
  , token   :: Token
  }
  deriving (Show)

parse :: [Token] -> Either ParseError [Stmt]
parse tokens = parse' tokens []
 where
  parse' :: [Token] -> [Stmt] -> Either ParseError [Stmt]
  parse' [(T.Eof, _)] stmts = Right (reverse stmts)
  parse' tokens' stmts = do
    (stmt, rest) <- statement tokens'
    parse' rest (stmt : stmts)

statement :: [Token] -> Either ParseError (Stmt, [Token])
statement [] = Left $ ParseError "Unexpected" (T.Eof, (0, 0))
statement tokens@(t : ts) =
  case fst t of
    T.Var -> declaration ts
    T.Print -> do
      (expr, ts') <- expression ts
      statement' ts' (A.Print expr)
    _ -> do
      (expr, ts') <- expression tokens
      statement' ts' (A.Expr expr)
 where
  statement' (t' : ts') stmt | fst t' == T.Semicolon = Right (stmt, ts')
  statement' tokens' _ = Left $ ParseError "Expected ';'" (head tokens')

  declaration ((T.Identifier name, _) : rest) = case rest of
    t' : ts'
      | fst t' == T.Equal ->
          expression ts' >>= \(expr, ts'') ->
            statement' ts'' (A.Var name expr)
    t' : _
      | fst t' == T.Semicolon ->
          statement' rest $ A.Var name $ Literal A.Nil
    _ -> Left $ ParseError "Expected '=' after var name." (head rest)
  declaration ts' = Left $ ParseError "Expected var name." (head ts')

---

type Parse = [Token] -> Either ParseError (Expr, [Token])

expression :: Parse
expression = assignment

assignment :: Parse
assignment tokens = do
  equality tokens >>= uncurry assignment'
 where
  assignment' expr (t@(T.Equal, _) : ts) = do
    (value, ts') <- assignment ts
    case expr of
      Variable name -> Right (A.Assignment name value, ts')
      _             -> Left $ ParseError "Invalid target." t
  assignment' expr ts = Right (expr, ts)

-- Binary

equality :: Parse
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
    recurse op =
      comparison ts >>= uncurry (equality' . Binary op expr)

comparison :: Parse
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
    recurse op =
      term ts >>= uncurry (comparison' . Binary op expr)

term :: Parse
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
    recurse op =
      factor ts >>= uncurry (term' . Binary op expr)

factor :: Parse
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
    recurse op =
      unary ts >>= uncurry (factor' . Binary op expr)

-- Unary

unary :: Parse
unary [] = primary []
unary tokens@(t : ts) =
  case fst t of
    T.Bang  -> recurse A.Bang
    T.Minus -> recurse A.Minus'
    _       -> primary tokens
 where
  recurse op =
    unary ts >>= Right . first (Unary op)

-- Primary

primary :: Parse
primary [] = Left $ ParseError "Unexpected" (T.Eof, (0, 0))
primary (t : ts) = case fst t of
  T.False' -> Right (Literal $ Bool' False, ts)
  T.True' -> Right (Literal $ Bool' True, ts)
  T.Nil -> Right (Literal A.Nil, ts)
  T.Number' n -> Right (Literal $ A.Number' n, ts)
  T.String' s -> Right (Literal $ A.String' s, ts)
  T.Identifier i -> Right (Variable i, ts)
  T.LeftParen -> do
    (expr, ts') <- expression ts
    case ts' of
      (T.RightParen, _) : ts'' -> Right (Grouping expr, ts'')
      _                        -> Left $ ParseError "Expected ')' after expr" t
  _ -> Left $ ParseError "Expected expr" t
