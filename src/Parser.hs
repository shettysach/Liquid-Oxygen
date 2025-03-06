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
parse = parse' []
 where
  parse' stmts [(T.Eof, _)] = Right (reverse stmts)
  parse' stmts tokens = declaration tokens >>= uncurry (parse' . (: stmts))

declaration :: [Token] -> Either ParseError (Stmt, [Token])
declaration [] = Left $ ParseError "Unexpected" (T.Eof, (0, 0))
declaration [t@(T.Eof, _)] = Left $ ParseError "Unexpected" t
declaration tokens@(t : ts) =
  case fst t of
    T.Var       -> varDeclaration ts
    T.Print     -> expression ts >>= uncurry (statement . A.Print)
    T.LeftBrace -> block [] ts >>= Right . first A.Block
    _           -> expression tokens >>= uncurry (statement . A.Expr)
 where
  statement stmt (t' : ts') | fst t' == T.Semicolon = Right (stmt, ts')
  statement _ tokens' = Left $ ParseError "Expected ';'" (head tokens')

  block stmts (t' : ts') | fst t' == T.RightBrace = Right (reverse stmts, ts')
  block stmts tokens' = declaration tokens' >>= uncurry (block . (: stmts))

  varDeclaration ((T.Identifier name, _) : rest) = case rest of
    t' : ts'
      | fst t' == T.Equal ->
          expression ts' >>= uncurry (statement . A.Var name)
    t' : _
      | fst t' == T.Semicolon ->
          statement (A.Var name $ Literal A.Nil) rest
    _ -> Left $ ParseError "Expected '=' after var name." (head rest)
  varDeclaration ts' = Left $ ParseError "Expected var name." (head ts')

---

type ParseExpr = [Token] -> Either ParseError (Expr, [Token])

expression :: ParseExpr
expression = assignment

assignment :: ParseExpr
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

equality :: ParseExpr
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

comparison :: ParseExpr
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

term :: ParseExpr
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

factor :: ParseExpr
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

unary :: ParseExpr
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

primary :: ParseExpr
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
