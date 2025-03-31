module Parser where

import Control.Arrow (first)
import Control.Monad (when)

import Error         (ParseError (ParseError))
import Syntax        as S
import Token         as T

parse :: [Token] -> Either ParseError [Stmt]
parse tokens = parse' ([], tokens)
 where
  parse' (stmts, [t]) | fst t == T.Eof = Right (reverse stmts)
  parse' (stmts, tokens') = declaration tokens' >>= (parse' . first (: stmts))

type Parse a = (a, [Token]) -> Either ParseError (a, [Token])
type Parser a = [Token] -> Either ParseError (a, [Token])

-- Stmts

declaration :: Parser Stmt
declaration (t : ts) = case fst t of
  T.Var       -> varDeclaration ts
  T.Print     -> expression ts >>= statement . first S.Print
  T.Fun       -> function ts
  T.Return    -> returnStatement ts
  T.LeftBrace -> first S.Block <$> block ([], ts)
  T.If        -> ifStatement ts
  T.While     -> while ts
  T.For       -> for ts
  _           -> expression (t : ts) >>= statement . first S.Expr
declaration tokens = expression tokens >>= statement . first S.Expr

statement :: Parse Stmt
statement (stmt, t : ts) | fst t == T.Semicolon = Right (stmt, ts)
statement (_, tokens) = Left $ ParseError "Expected ';'" (head tokens)

block :: Parse [Stmt]
block (stmts, t : ts) | fst t == T.RightBrace = Right (reverse stmts, ts)
block (stmts, tokens) = declaration tokens >>= block . first (: stmts)

varDeclaration :: Parser Stmt
varDeclaration ((T.Identifier name, pos) : t : ts)
  | fst t == T.Equal = expression ts >>= (statement . first (S.Var (name, pos) . Just))
  | fst t == T.Semicolon = statement (S.Var (name, pos) Nothing, t : ts)
  | otherwise = Left $ ParseError "Expected = after var name" t
varDeclaration tokens = Left $ ParseError "Expected var name" (head tokens)

ifStatement :: Parser Stmt
ifStatement (t : ts) | fst t == T.LeftParen = do
  (condition, afterCondn) <- expression ts
  (thenBranch, afterThen) <- case afterCondn of
    t' : ts' | fst t' == T.RightParen -> declaration ts'
    _ -> Left $ ParseError "Expected ')' after if condition" (head afterCondn)
  (elseBranch, afterElse) <- case afterThen of
    t' : ts' | fst t' == T.Else -> first Just <$> declaration ts'
    _                           -> Right (Nothing, afterThen)
  Right (S.If condition thenBranch elseBranch, afterElse)
ifStatement tokens = Left $ ParseError "Expected '(' after 'if'" (head tokens)

while :: Parser Stmt
while (t : ts) | fst t == T.LeftParen = do
  (condition, afterCondn) <- expression ts
  case afterCondn of
    t' : ts' | fst t' == T.RightParen -> first (S.While condition) <$> declaration ts'
    _ -> Left $ ParseError "Expected ')' after while condition" (head ts)
while tokens = Left $ ParseError "Expected '(' after 'while'" (head tokens)

for :: Parser Stmt
for (t : ts) | fst t == T.LeftParen = do
  (initializer, afterInit) <- case ts of
    t1 : ts1
      | fst t1 == T.Semicolon -> Right (Nothing, ts1)
      | fst t1 == T.Var -> first Just <$> varDeclaration ts1
    _ -> do
      (expr, after) <- expression ts
      case after of
        t2 : ts2 | fst t2 == T.Semicolon -> Right (Just (Expr expr), ts2)
        _ -> Left $ ParseError "Expected ';' after loop initializer" (head after)

  (condition, afterCondn) <- case afterInit of
    t1 : ts1 | fst t1 == T.Semicolon -> Right (Literal (Bool' True), ts1)
    _ -> do
      (expr, after) <- expression afterInit
      case after of
        t2 : ts2 | fst t2 == T.Semicolon -> Right (expr, ts2)
        _ -> Left $ ParseError "Expected ';' after loop condition" (head after)

  (increment, afterInc) <- case afterCondn of
    t1 : ts1 | fst t1 == T.RightParen -> Right (Nothing, ts1)
    _ -> do
      (expr, after) <- expression afterCondn
      case after of
        t2 : ts2 | fst t2 == T.RightParen -> Right (Just expr, ts2)
        _ -> Left $ ParseError "Expected ')' after increment" (head after)

  (stmt, afterStmt) <- declaration afterInc

  let body = case increment of
        Nothing  -> stmt
        Just inc -> Block [stmt, Expr inc]
      stmt' = S.While condition body
      loop = case initializer of
        Nothing    -> stmt'
        Just init' -> Block [init', stmt']
  Right (loop, afterStmt)
for tokens = Left $ ParseError "Expected '(' after 'for'" (head tokens)

-- TODO: Handle kind (method/fn)
function :: Parser Stmt
function (t0 : t1 : ts)
  | (T.Identifier name, pos) <- t0
  , T.LeftParen <- fst t1 = do
      (params, afterParams) <- parameters [] ts
      when
        (length params >= 255)
        (Left $ ParseError ">= 255 params" $ head afterParams)

      (body, afterBody) <- case afterParams of
        (t' : ts') | fst t' == T.LeftBrace -> block ([], ts')
        _ -> Left $ ParseError "Expected '{' after params" (head afterParams)

      Right (S.Fun (name, pos) params body, afterBody)
function tokens = Left $ ParseError "Expected identifier" (head tokens)

parameters :: [Name] -> Parser [Name]
parameters ps (t : ts)
  | T.RightParen <- fst t = Right (reverse ps, ts)
  | T.Comma <- fst t = parameters ps ts
  | (T.Identifier param, pos) <- t = parameters ((param, pos) : ps) ts
parameters _ tokens = Left $ ParseError "Expected ')', ',' or identifier" (head tokens)

returnStatement :: Parser Stmt
returnStatement (t : ts) | fst t == T.Semicolon = Right (S.Return Nothing, ts)
returnStatement tokens = do
  (expr, after) <- expression tokens
  case after of
    t : ts | fst t == T.Semicolon -> Right (S.Return (Just expr), ts)
    _                             -> Left $ ParseError "Expected ';'" (head after)

-- Expr

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment tokens = Parser.or tokens >>= assignment'
 where
  assignment' (expr, t : ts)
    | fst t == T.Equal =
        assignment ts >>= \(value, ts') ->
          case expr of
            Variable var -> Right (S.Assignment var value, ts')
            _            -> Left (ParseError "Invalid target" t)
  assignment' (expr, ts) = Right (expr, ts)

or :: Parser Expr
or tokens =
  Parser.and tokens >>= or'
 where
  or' (expr, []) = Right (expr, [])
  or' (expr, t : ts) =
    case fst t of
      T.Or -> loop S.Or
      _    -> Right (expr, t : ts)
   where
    loop op =
      Parser.and ts >>= or' . first (Logical (op, snd t) expr)

and :: Parser Expr
and tokens =
  equality tokens >>= and'
 where
  and' (expr, []) = Right (expr, [])
  and' (expr, t : ts) =
    case fst t of
      T.And -> loop S.And
      _     -> Right (expr, t : ts)
   where
    loop op =
      equality ts >>= and' . first (Logical (op, snd t) expr)

equality :: Parser Expr
equality tokens = comparison tokens >>= equality'
 where
  equality' (expr, []) = Right (expr, [])
  equality' (expr, t : ts) = case fst t of
    T.BangEqual  -> loop S.BangEqual
    T.EqualEqual -> loop S.EqualEqual
    _            -> Right (expr, t : ts)
   where
    loop op =
      comparison ts >>= equality' . first (Binary (op, snd t) expr)

comparison :: Parser Expr
comparison tokens = term tokens >>= comparison'
 where
  comparison' (expr, []) = Right (expr, [])
  comparison' (expr, t : ts) = case fst t of
    T.Greater      -> loop S.Greater
    T.GreaterEqual -> loop S.GreaterEqual
    T.Less         -> loop S.Less
    T.LessEqual    -> loop S.LessEqual
    _              -> Right (expr, t : ts)
   where
    loop op =
      term ts >>= comparison' . first (Binary (op, snd t) expr)

term :: Parser Expr
term tokens = factor tokens >>= term'
 where
  term' (expr, []) = Right (expr, [])
  term' (expr, t : ts) = case fst t of
    T.Minus -> loop S.Minus
    T.Plus  -> loop S.Plus
    _       -> Right (expr, t : ts)
   where
    loop op =
      factor ts >>= term' . first (Binary (op, snd t) expr)

factor :: Parser Expr
factor tokens = unary tokens >>= factor'
 where
  factor' (expr, []) = Right (expr, [])
  factor' (expr, t : ts) = case fst t of
    T.Slash -> loop S.Slash
    T.Star  -> loop S.Star
    _       -> Right (expr, t : ts)
   where
    loop op =
      unary ts >>= factor' . first (Binary (op, snd t) expr)

unary :: Parser Expr
unary [] = call []
unary (t : ts) =
  case fst t of
    T.Bang  -> (first . Unary) (S.Bang, snd t) <$> unary ts
    T.Minus -> (first . Unary) (S.Minus', snd t) <$> unary ts
    _       -> call (t : ts)

call :: Parser Expr
call tokens = primary tokens >>= call'
 where
  call' (expr, []) = Right (expr, [])
  call' (expr, t : ts) = case fst t of
    T.LeftParen -> finish expr ts >>= call'
    _           -> Right (expr, t : ts)

  finish expr' tokens' = do
    (args, rest) <- arguments tokens'
    when
      (length args >= 255)
      (Left $ ParseError ">= 255 args" $ head rest)
    case rest of
      t' : ts' | fst t' == T.RightParen -> Right (Call expr' args, ts')
      _                                 -> Left $ ParseError "Expected ')' after args" (head rest)

  arguments (t' : ts') | fst t' == T.RightParen = Right ([], t' : ts')
  arguments tokens' = do
    (arg, rest) <- expression tokens'
    case rest of
      (t' : ts') | fst t' == T.Comma -> first (arg :) <$> arguments ts'
      _                              -> Right ([arg], rest)

primary :: Parser Expr
primary (t : ts) = case fst t of
  T.False' -> Right (Literal $ Bool' False, ts)
  T.True' -> Right (Literal $ Bool' True, ts)
  T.Nil -> Right (Literal S.Nil, ts)
  T.Number' n -> Right (Literal $ S.Number' n, ts)
  T.String' s -> Right (Literal $ S.String' s, ts)
  T.Identifier i -> Right (Variable (i, snd t), ts)
  T.LeftParen -> do
    (expr, rest) <- expression ts
    case rest of
      t' : ts' | fst t' == T.RightParen -> Right (Grouping expr, ts')
      _                                 -> Left $ ParseError "Expected ')' after expr" t
  _ -> Left $ ParseError "Expected expr" t
primary tokens = Left $ ParseError "Expected expr" (head tokens)
