module Parser where

import Control.Arrow (first)
import Data.Functor  ((<&>))

import AST           as A
import Token         as T

data ParseError = ParseError
  { message :: String
  , token   :: Token
  }
  deriving (Show)

type Parse a = [Token] -> Either ParseError (a, [Token])

parse :: [Token] -> Either ParseError [Stmt]
parse = parse' []
 where
  parse' stmts [(T.Eof, _)] = Right (reverse stmts)
  parse' stmts tokens       = statement tokens >>= uncurry (parse' . (: stmts))

statement :: Parse Stmt
statement [] = Left $ ParseError "Unexpected" (T.Eof, (0, 0))
statement [t@(T.Eof, _)] = Left $ ParseError "Unexpected" t
statement tokens@(t : ts) =
  case fst t of
    T.Var       -> declaration ts
    T.Print     -> expression ts >>= uncurry (statement' . A.Print)
    T.LeftBrace -> block [] ts <&> first A.Block
    T.If        -> ifStatement ts
    T.While     -> while ts
    T.For       -> for ts
    _           -> expression tokens >>= uncurry (statement' . A.Expr)

statement' :: Stmt -> Parse Stmt
statement' stmt (t : ts) | fst t == T.Semicolon = Right (stmt, ts)
statement' _ tokens = Left $ ParseError "Expected ;" (head tokens)

block :: [Stmt] -> Parse [Stmt]
block stmts (t : ts) | fst t == T.RightBrace = Right (reverse stmts, ts)
block stmts tokens = statement tokens >>= uncurry (block . (: stmts))

declaration :: Parse Stmt
declaration ((T.Identifier name, _) : rest) = case rest of
  t : ts
    | fst t == T.Equal ->
        expression ts >>= uncurry (statement' . A.Var name)
  t : _
    | fst t == T.Semicolon ->
        statement' (A.Var name $ Literal A.Nil) rest
  _ -> Left $ ParseError "Expected = after var name." (head rest)
declaration ts = Left $ ParseError "Expected var name." (head ts)

ifStatement :: Parse Stmt
ifStatement (token : after) | fst token == T.LeftParen = do
  (condition, afterCondn) <- expression after
  case afterCondn of
    t : ts | fst t == T.RightParen -> do
      (thenBranch, afterThen) <- statement ts
      (elseBranch, afterElse) <- case afterThen of
        (T.Else, _) : ts -> statement ts <&> first Just
        _                -> Right (Nothing, afterThen)
      Right (A.If condition thenBranch elseBranch, afterElse)
    _ -> Left $ ParseError "Expected ')' after if condition." (head afterCondn)
ifStatement tokens = Left $ ParseError "Expected '(' after 'if'." (head tokens)

while :: Parse Stmt
while (token : after) | fst token == T.LeftParen = do
  (condition, afterCondn) <- expression after
  case afterCondn of
    t : ts | fst t == T.RightParen -> statement ts <&> first (A.While condition)
    _ -> Left $ ParseError "Expected ')' after while condition." (head after)
while tokens = Left $ ParseError "Expected '(' after 'while'." (head tokens)

for :: Parse Stmt
for ((T.LeftParen, _) : after) = do
  (initializer, afterInit) <- case after of
    t : ts
      | fst t == T.Semicolon -> return (Nothing, ts)
      | fst t == T.Var -> declaration ts <&> first Just
    _ -> do
      (expr, rest) <- expression after
      case rest of
        (T.Semicolon, _) : ts' -> return (Just (Expr expr), ts')
        _ -> Left $ ParseError "Expected ';' after loop initializer" (head rest)

  (condition, afterCondn) <- case afterInit of
    t : ts | fst t == T.Semicolon -> return (Literal (Bool' True), ts)
    _ -> do
      (condExpr, rest) <- expression afterInit
      case rest of
        t' : ts' | fst t' == T.Semicolon -> return (condExpr, ts')
        _ -> Left $ ParseError "Expected ';' after loop condition" (head rest)

  (increment, afterInc) <- case afterCondn of
    t : ts | fst t == T.RightParen -> return (Nothing, ts)
    _ -> do
      (incExpr, afterInc) <- expression afterCondn
      case afterInc of
        (T.RightParen, _) : ts' -> return (Just incExpr, ts')
        _ -> Left $ ParseError "Expected ')' after increment" (head afterInc)
  (stmt, afterStmt) <- statement afterInc

  let body = case increment of
        Nothing  -> stmt
        Just inc -> Block [stmt, Expr inc]
  let stmt' = A.While condition body
  let loop = case initializer of
        Nothing    -> stmt'
        Just init' -> Block [init', stmt']
  return (loop, afterStmt)
for tokens = Left $ ParseError "Expected '(' after 'for'." (head tokens)

-- Expr

expression :: Parse Expr
expression = assignment

assignment :: Parse Expr
assignment tokens = do
  Parser.or tokens >>= uncurry assignment'
 where
  assignment' expr (t@(T.Equal, _) : ts) = do
    (value, ts') <- assignment ts
    case expr of
      Variable name -> Right (A.Assignment name value, ts')
      _             -> Left $ ParseError "Invalid target." t
  assignment' expr ts = Right (expr, ts)

-- Logical

or :: Parse Expr
or tokens = do
  Parser.and tokens >>= uncurry or'
 where
  or' expr [] = Right (expr, [])
  or' expr tokens'@(t : ts) =
    case fst t of
      T.Or -> recurse A.Or
      _    -> Right (expr, tokens')
   where
    recurse op =
      comparison ts >>= uncurry (or' . Logical op expr)

and :: Parse Expr
and tokens = do
  equality tokens >>= uncurry and'
 where
  and' expr [] = Right (expr, [])
  and' expr tokens'@(t : ts) =
    case fst t of
      T.And -> recurse A.And
      _     -> Right (expr, tokens')
   where
    recurse op =
      comparison ts >>= uncurry (and' . Logical op expr)

-- Binary

equality :: Parse Expr
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

comparison :: Parse Expr
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

term :: Parse Expr
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

factor :: Parse Expr
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

unary :: Parse Expr
unary [] = primary []
unary tokens@(t : ts) =
  case fst t of
    T.Bang  -> unary ts <&> (first . Unary) A.Bang
    T.Minus -> unary ts <&> (first . Unary) A.Minus'
    _       -> primary tokens

-- Primary

primary :: Parse Expr
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
