{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module Parser where

import Control.Arrow (first)
import Control.Monad (when)

import Data.Function ((&))
import Error         (ParseError (ParseError))
import Syntax        as S
import Token         as T

parse :: [Token] -> Either ParseError [Stmt]
parse tokens = parse' ([], tokens)
 where
  parse' (stmts, [fst -> T.Eof]) = Right (reverse stmts)
  parse' (stmts, tokens')        = declaration tokens' >>= parse' . first (: stmts)

type Parse a = (a, [Token]) -> Either ParseError (a, [Token])
type Parser a = [Token] -> Either ParseError (a, [Token])

-- Stmts

declaration :: Parser Stmt
declaration (t : ts) = case fst t of
  T.Var       -> varDeclaration ts
  T.Fun       -> function ts
  T.Class     -> classDeclaration ts
  T.Return    -> returnStatement (snd t) ts
  T.LeftBrace -> first S.Block <$> block ([], ts)
  T.If        -> ifStatement ts
  T.While     -> while ts
  T.For       -> for ts
  T.Print     -> expression ts >>= statement . first S.Print
  _           -> expression (t : ts) >>= statement . first S.Expr
declaration tokens = expression tokens >>= statement . first S.Expr

statement :: Parse Stmt
statement (stmt, (fst -> T.Semicolon) : ts) = Right (stmt, ts)
statement (_, tokens)                       = Left $ ParseError "Expected ';'" $ head tokens

block :: Parse [Stmt]
block (stmts, (fst -> T.RightBrace) : ts) = Right (reverse stmts, ts)
block (stmts, tokens)                     = declaration tokens >>= block . first (: stmts)

varDeclaration :: Parser Stmt
varDeclaration ((T.Identifier name, pos) : t : ts) = case fst t of
  T.Equal     -> expression ts >>= statement . first (S.Var (name, pos) . Just)
  T.Semicolon -> statement (S.Var (name, pos) Nothing, t : ts)
  _           -> Left $ ParseError "Expected = after var name" t
varDeclaration tokens = Left $ ParseError "Expected var name" $ head tokens

ifStatement :: Parser Stmt
ifStatement ((fst -> T.LeftParen) : ts) = do
  (condition, afterCondn) <- expression ts

  (thenBranch, afterThen) <-
    afterCondn & case fst $ head afterCondn of
      T.RightParen -> declaration . tail
      _            -> Left . ParseError "Expected '' after if condition" . head

  (elseBranch, afterElse) <-
    afterThen & case fst $ head afterThen of
      T.Else -> fmap (first Just) . declaration . tail
      _      -> Right . (Nothing,)

  Right (S.If condition thenBranch elseBranch, afterElse)
ifStatement tokens = Left $ ParseError "Expected '(' after 'if'" $ head tokens

while :: Parser Stmt
while ((fst -> T.LeftParen) : ts) = do
  (condition, afterCondn) <- expression ts
  afterCondn & case fst $ head afterCondn of
    T.RightParen -> fmap (first $ S.While condition) . declaration . tail
    _            -> Left . ParseError "Expected ')' after while condition" . head
while tokens = Left $ ParseError "Expected '(' after 'while'" $ head tokens

for :: Parser Stmt
for ((fst -> T.LeftParen) : ts) = do
  (initializer, afterInit) <- case fst $ head ts of
    T.Semicolon -> Right (Nothing, tail ts)
    T.Var -> first Just <$> varDeclaration (tail ts)
    _ -> do
      (expr, afterExpr) <- expression ts
      afterExpr & case fst $ head afterExpr of
        T.Semicolon -> Right . (Just (Expr expr),) . tail
        _           -> Left . ParseError "Expected ';' after loop initializer" . head

  (condition, afterCondn) <- case fst $ head afterInit of
    T.Semicolon -> Right (Literal (Bool' True), tail afterInit)
    _ -> do
      (expr, afterExpr) <- expression afterInit
      afterExpr & case fst $ head afterExpr of
        T.Semicolon -> Right . (expr,) . tail
        _           -> Left . ParseError "Expected ';' after loop condition" . head

  (increment, afterInc) <- case fst $ head afterCondn of
    T.RightParen -> Right (Nothing, tail afterCondn)
    _ -> do
      (expr, afterExpr) <- expression afterCondn
      afterExpr & case fst $ head afterExpr of
        T.RightParen -> Right . (Just expr,) . tail
        _            -> Left . ParseError "Expected ')' after increment" . head

  (stmt, afterStmt) <- declaration afterInc

  let body = case increment of
        Nothing  -> stmt
        Just inc -> Block [stmt, Expr inc]
      stmt' = S.While condition body
      loop = case initializer of
        Nothing    -> stmt'
        Just init' -> Block [init', stmt']

  Right (loop, afterStmt)
for tokens = Left $ ParseError "Expected '(' after 'for'" $ head tokens

function :: Parser Stmt
function ((T.Identifier name, pos) : (fst -> T.LeftParen) : ts) = do
  (params, afterParams) <- parameters [] ts
  when (length params >= 255) (Left $ ParseError ">= 255 params" $ head afterParams)

  (body, afterBody) <-
    afterParams & case fst $ head afterParams of
      T.LeftBrace -> block . ([],) . tail
      _           -> Left . ParseError "Expected '{' after params" . head

  Right (S.Function (name, pos) params body, afterBody)
function tokens = Left $ ParseError "Expected identifier" $ head tokens

parameters :: [String'] -> Parser [String']
parameters ps ((fst -> T.RightParen) : rest)     = Right (ps, rest)
parameters ps ((fst -> T.Comma) : rest)          = parameters ps rest
parameters ps ((T.Identifier param, pos) : rest) = parameters ((param, pos) : ps) rest
parameters _ tokens                              = Left $ ParseError "Expected ')', ',' or identifier" $ head tokens

returnStatement :: (Int, Int) -> Parser Stmt
returnStatement pos ((fst -> T.Semicolon) : rest) = Right (S.Return (Nothing, pos), rest)
returnStatement pos tokens = do
  (expr, afterExpr) <- expression tokens
  afterExpr & case fst $ head afterExpr of
    T.Semicolon -> Right . (S.Return (Just expr, pos),) . tail
    _           -> Left . ParseError "Expected ';' after return" . head

classDeclaration :: Parser Stmt
classDeclaration (t : ts) | T.Identifier name <- fst t = do
  (super, afterSuper) <- case ts of
    (fst -> T.Less) : (T.Identifier name', pos) : ts' -> Right (Just (S.Variable (name', pos)), ts')
    (fst -> T.LeftBrace) : _                          -> Right (Nothing, ts)
    (fst -> T.Less) : ts'                             -> Left $ ParseError "Expected superclass after <" (head ts')
    _                                                 -> Left $ ParseError "Expected '<' or '{'" (head ts)

  (methods, afterMethods) <-
    afterSuper & case fst $ head afterSuper of
      T.LeftBrace -> method . ([],) . tail
      _           -> Left . ParseError "Expected '{' before class body" . head

  Right (S.Class (name, snd t) super methods, afterMethods)
classDeclaration tokens = Left $ ParseError "Expected class name" $ head tokens

method :: Parse [Stmt]
method (mthds, t : ts) | T.RightBrace <- fst t = Right (reverse mthds, ts)
method (mthds, tokens) = function tokens >>= method . first (: mthds)

-- Expr

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment tokens = Parser.or tokens >>= assignment'
 where
  assignment' (expr, t : ts) | T.Equal <- fst t = do
    (value, after) <- assignment ts
    case expr of
      Variable var   -> Right (S.Assignment var value, after)
      Get expr' prop -> Right (S.Set expr' prop value, after)
      _              -> Left (ParseError "Invalid target" t)
  assignment' (expr, tokens') = Right (expr, tokens')

or :: Parser Expr
or tokens = Parser.and tokens >>= or'
 where
  or' (expr, tokens') = case ttype of
    T.Or -> recurse S.Or
    _    -> Right (expr, tokens')
   where
    (ttype, pos) = head tokens'
    recurse op = Parser.and (tail tokens') >>= or' . first (Logical (op, pos) expr)

and :: Parser Expr
and tokens = equality tokens >>= and'
 where
  and' (expr, tokens') = case ttype of
    T.And -> recurse S.And
    _     -> Right (expr, tokens')
   where
    (ttype, pos) = head tokens'
    recurse op = equality (tail tokens') >>= and' . first (Logical (op, pos) expr)

equality :: Parser Expr
equality tokens = comparison tokens >>= equality'
 where
  equality' (expr, tokens') = case ttype of
    T.BangEqual  -> recurse S.BangEqual
    T.EqualEqual -> recurse S.EqualEqual
    _            -> Right (expr, tokens')
   where
    (ttype, pos) = head tokens'
    recurse op = comparison (tail tokens') >>= equality' . first (Binary (op, pos) expr)

comparison :: Parser Expr
comparison tokens = term tokens >>= comparison'
 where
  comparison' (expr, tokens') = case ttype of
    T.Greater      -> recurse S.Greater
    T.GreaterEqual -> recurse S.GreaterEqual
    T.Less         -> recurse S.Less
    T.LessEqual    -> recurse S.LessEqual
    _              -> Right (expr, tokens')
   where
    (ttype, pos) = head tokens'
    recurse op = term (tail tokens') >>= comparison' . first (Binary (op, pos) expr)

term :: Parser Expr
term tokens = factor tokens >>= term'
 where
  term' (expr, tokens') = case ttype of
    T.Minus -> recurse S.Minus
    T.Plus  -> recurse S.Plus
    _       -> Right (expr, tokens')
   where
    (ttype, pos) = head tokens'
    recurse op = factor (tail tokens') >>= term' . first (Binary (op, pos) expr)

factor :: Parser Expr
factor tokens = unary tokens >>= factor'
 where
  factor' (expr, tokens') = case fst t of
    T.Slash -> recurse S.Slash
    T.Star  -> recurse S.Star
    _       -> Right (expr, tokens')
   where
    t = head tokens'
    recurse op = unary (tail tokens') >>= factor' . first (Binary (op, snd t) expr)

unary :: Parser Expr
unary [] = call []
unary (t : ts) = case fst t of
  T.Bang  -> (first . Unary) (S.Bang, snd t) <$> unary ts
  T.Minus -> (first . Unary) (S.Minus', snd t) <$> unary ts
  _       -> call (t : ts)

call :: Parser Expr
call tokens = primary tokens >>= call'
 where
  call' (expr, []) = Right (expr, [])
  call' (expr, t : ts) = case fst t of
    T.LeftParen -> finish expr ts >>= call'
    T.Dot -> case ts of
      (T.Identifier prop, pos) : ts' -> call' (Get expr (prop, pos), ts')
      _                              -> Left $ ParseError "Expected prop/method name after '.'" $ head ts
    _ -> Right (expr, t : ts)

  finish expr tokens' = do
    (args, rest) <- arguments tokens'
    when (length args >= 255) (Left $ ParseError ">= 255 args" $ head rest)
    rest & case fst $ head rest of
      T.RightParen -> Right . (Call (expr, (snd . head) tokens) args,) . tail
      _            -> Left . ParseError "Expected ')' after args" . head

  arguments tokens'@(fst . head -> T.RightParen) = Right ([], tokens')
  arguments tokens' = do
    (arg, rest) <- expression tokens'
    rest & case fst $ head rest of
      T.Comma -> fmap (first (arg :)) . arguments . tail
      _       -> Right . ([arg],)

primary :: Parser Expr
primary (t : ts) = case fst t of
  T.False'       -> Right (Literal $ Bool' False, ts)
  T.True'        -> Right (Literal $ Bool' True, ts)
  T.Nil          -> Right (Literal S.Nil, ts)
  T.Number' n    -> Right (Literal $ S.Number' n, ts)
  T.String' s    -> Right (Literal $ S.String' s, ts)
  T.Identifier i -> Right (Variable (i, snd t), ts)
  T.This         -> Right (S.This (snd t), ts)
  T.Super        -> superExpr t ts
  T.LeftParen    -> grouping t ts
  _              -> Left $ ParseError "Expected expr" t
primary tokens = Left $ ParseError "Expected expr" $ head tokens

superExpr :: Token -> Parser Expr
superExpr t ts = case ts of
  (fst -> T.Dot) : (T.Identifier name, pos) : rest -> Right (S.Super (snd t) (name, pos), rest)
  (fst -> T.Dot) : rest -> Left $ ParseError "Expected superclass method name after '.'" $ head rest
  _ -> Left $ ParseError "Expected '.' after 'super'" $ head ts

grouping :: Token -> Parser Expr
grouping t ts = do
  (expr, rest) <- expression ts
  case fst $ head rest of
    T.RightParen -> Right (Grouping expr, tail rest)
    _            -> Left $ ParseError "Expected ')' after expr" t
