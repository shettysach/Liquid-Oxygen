{-# LANGUAGE ViewPatterns #-}

module Parser where

import Control.Arrow      (first)
import Control.Monad      (when)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, head)
import Prelude            hiding (head)

import Error              (ParseError (ParseError))
import Position           (Position)
import Syntax             as S
import Token              as T

parse :: NonEmpty Token -> Either ParseError [Stmt]
parse tokens = parse' ([], tokens)
 where
  parse' (stmts, fst . head -> T.Eof) = Right (reverse stmts)
  parse' (stmts, tokens')             = declaration tokens' >>= parse' . first (: stmts)

type Parse a = (a, NonEmpty Token) -> Either ParseError (a, NonEmpty Token)
type Parser a = NonEmpty Token -> Either ParseError (a, NonEmpty Token)

-- Stmts

declaration :: Parser Stmt
declaration (t :| ts) = let ts' = fromList ts in case fst t of
  T.Var       -> varDeclaration ts'
  T.Fun       -> first S.Function <$> function ts'
  T.Class     -> classDeclaration ts'
  T.Return    -> returnStatement (snd t) ts'
  T.LeftBrace -> first S.Block <$> block ([], ts')
  T.If        -> ifStatement ts'
  T.While     -> while ts'
  T.For       -> for ts'
  T.Print     -> expression ts' >>= statement . first S.Print
  _           -> expression (t :| ts) >>= statement . first S.Expr

statement :: Parse Stmt
statement (stmt, (fst -> T.Semicolon) :| ts) = Right (stmt, fromList ts)
statement (_, t :| _)                        = Left $ ParseError "Expected ';'" t

block :: Parse [Stmt]
block (stmts, (fst -> T.RightBrace) :| ts) = Right (reverse stmts, fromList ts)
block (stmts, tokens)                      = declaration tokens >>= block . first (: stmts)

varDeclaration :: Parser Stmt
varDeclaration ((T.Identifier name, pos) :| t : ts) = case fst t of
  T.Equal     -> expression (fromList ts) >>= statement . first (S.Var (name, pos) . Just)
  T.Semicolon -> statement (S.Var (name, pos) Nothing, t :| ts)
  _           -> Left $ ParseError "Expected = after var name" t
varDeclaration (t :| _) = Left $ ParseError "Expected var name" t

ifStatement :: Parser Stmt
ifStatement ((fst -> T.LeftParen) :| ts) = do
  (condition, t1 :| ts1) <- expression $ fromList ts

  (thenBranch, t2 :| ts2) <-
    case fst t1 of
      T.RightParen -> declaration $ fromList ts1
      _            -> Left $ ParseError "Expected '' after if condition" t1

  (elseBranch, afterElse) <-
    case fst t2 of
      T.Else -> first Just <$> declaration (fromList ts2)
      _      -> Right (Nothing, t2 :| ts2)

  Right (S.If condition thenBranch elseBranch, afterElse)
ifStatement (t :| _) = Left $ ParseError "Expected '(' after 'if'" t

while :: Parser Stmt
while ((fst -> T.LeftParen) :| afterWhile) = do
  (condition, t :| ts) <- expression $ fromList afterWhile
  case fst t of
    T.RightParen -> first (S.While condition) <$> declaration (fromList ts)
    _            -> Left $ ParseError "Expected ')' after while condition" t
while (t :| _) = Left $ ParseError "Expected '(' after 'while'" t

for :: Parser Stmt
for ((fst -> T.LeftParen) :| t : ts) = do
  (initializer, t1 :| ts1) <- case fst t of
    T.Semicolon -> Right (Nothing, fromList ts)
    T.Var -> first Just <$> varDeclaration (fromList ts)
    _ -> do
      (expr, t' :| ts') <- expression $ t :| ts
      case fst t' of
        T.Semicolon -> Right (Just (Expr expr), fromList ts')
        _           -> Left $ ParseError "Expected ';' after loop initializer" t'

  (condition, t2 :| ts2) <- case fst t1 of
    T.Semicolon -> Right (Literal (Bool' True), fromList ts1)
    _ -> do
      (expr, t' :| ts') <- expression $ t1 :| ts1
      case fst t' of
        T.Semicolon -> Right (expr, fromList ts')
        _           -> Left $ ParseError "Expected ';' after loop condition" t'

  (increment, afterInc) <- case fst t2 of
    T.RightParen -> Right (Nothing, fromList ts2)
    _ -> do
      (expr, t' :| ts') <- expression $ t2 :| ts2
      case fst t' of
        T.RightParen -> Right (Just expr, fromList ts')
        _            -> Left $ ParseError "Expected ')' after increment" t'

  (stmt, afterStmt) <- declaration afterInc

  let body = case increment of
        Nothing  -> stmt
        Just inc -> Block [stmt, Expr inc]
      stmt' = S.While condition body
      loop = case initializer of
        Nothing    -> stmt'
        Just init' -> Block [init', stmt']

  Right (loop, afterStmt)
for (t :| _) = Left $ ParseError "Expected '(' after 'for'" t

function :: Parser FnStmt
function ((T.Identifier name, pos) :| (fst -> T.LeftParen) : rest) = do
  (params, t :| ts) <- parameters [] (fromList rest)
  when (length params >= 255) (Left $ ParseError ">= 255 params" t)

  (body, afterBody) <-
    case fst t of
      T.LeftBrace -> block ([], fromList ts)
      _           -> Left $ ParseError "Expected '{' after params" t

  Right (S.FnStmt (name, pos) params body, afterBody)
function (t :| _) = Left $ ParseError "Expected identifier" t

parameters :: [String'] -> Parser [String']
parameters ps ((fst -> T.RightParen) :| rest)     = Right (ps, fromList rest)
parameters ps ((fst -> T.Comma) :| rest)          = parameters ps (fromList rest)
parameters ps ((T.Identifier param, pos) :| rest) = parameters ((param, pos) : ps) (fromList rest)
parameters _ (t :| _)                             = Left $ ParseError "Expected ')', ',' or identifier" t

returnStatement :: Position -> Parser Stmt
returnStatement pos ((fst -> T.Semicolon) :| rest) = Right (S.Return (Nothing, pos), fromList rest)
returnStatement pos tokens = do
  (expr, t :| ts) <- expression tokens
  case fst t of
    T.Semicolon -> Right (S.Return (Just expr, pos), fromList ts)
    _           -> Left $ ParseError "Expected ';' after return" t

classDeclaration :: Parser Stmt
classDeclaration ((T.Identifier name, pos) :| ts) = do
  (super, afterSuper) <- case fromList ts of
    (fst -> T.Less) :| (T.Identifier name', pos') : rest -> Right (Just (S.Variable (name', pos')), rest)
    (fst -> T.LeftBrace) :| _                            -> Right (Nothing, ts)
    (fst -> T.Less) :| t' : _                            -> Left $ ParseError "Expected superclass after '<'" t'
    ts'                                                  -> Left $ ParseError "Expected '<' or '{'" (head ts')

  case fromList afterSuper of
    (T.LeftBrace, _) :| rest -> first (S.Class (name, pos) super) <$> method ([], fromList rest)
    afterSuper'              -> Left $ ParseError "Expected '{' before class body" (head afterSuper')
classDeclaration (t :| _) = Left $ ParseError "Expected class name" t

method :: ([FnStmt], NonEmpty Token) -> Either ParseError ([FnStmt], NonEmpty Token)
method (mthds, t :| ts) | T.RightBrace <- fst t = Right (reverse mthds, fromList ts)
method (mthds, tokens) = function tokens >>= method . first (: mthds)

-- Expr

expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment tokens = Parser.or tokens >>= assignment'
 where
  assignment' (expr, t :| ts) | T.Equal <- fst t = do
    (value, after) <- assignment (fromList ts)
    case expr of
      Variable var   -> Right (S.Assignment var value, after)
      Get expr' prop -> Right (S.Set expr' prop value, after)
      _              -> Left (ParseError "Invalid target" t)
  assignment' (expr, tokens') = Right (expr, tokens')

or :: Parser Expr
or tokens = Parser.and tokens >>= or'
 where
  or' (expr, t :| ts) = case fst t of
    T.Or -> recurse S.Or
    _    -> Right (expr, t :| ts)
   where
    recurse op = Parser.and (fromList ts) >>= or' . first (Logical (op, snd t) expr)

and :: Parser Expr
and tokens = equality tokens >>= and'
 where
  and' (expr, t :| ts) = case fst t of
    T.And -> recurse S.And
    _     -> Right (expr, t :| ts)
   where
    recurse op = equality (fromList ts) >>= and' . first (Logical (op, snd t) expr)

equality :: Parser Expr
equality tokens = comparison tokens >>= equality'
 where
  equality' (expr, t :| ts) = case fst t of
    T.BangEqual  -> recurse S.BangEqual
    T.EqualEqual -> recurse S.EqualEqual
    _            -> Right (expr, t :| ts)
   where
    recurse op = comparison (fromList ts) >>= equality' . first (Binary (op, snd t) expr)

comparison :: Parser Expr
comparison tokens = term tokens >>= comparison'
 where
  comparison' (expr, t :| ts) = case fst t of
    T.Greater      -> recurse S.Greater
    T.GreaterEqual -> recurse S.GreaterEqual
    T.Less         -> recurse S.Less
    T.LessEqual    -> recurse S.LessEqual
    _              -> Right (expr, t :| ts)
   where
    recurse op = term (fromList ts) >>= comparison' . first (Binary (op, snd t) expr)

term :: Parser Expr
term tokens = factor tokens >>= term'
 where
  term' (expr, t :| ts) = case fst t of
    T.Minus -> recurse S.Minus
    T.Plus  -> recurse S.Plus
    _       -> Right (expr, t :| ts)
   where
    recurse op = factor (fromList ts) >>= term' . first (Binary (op, snd t) expr)

factor :: Parser Expr
factor tokens = unary tokens >>= factor'
 where
  factor' (expr, t :| ts) = case fst t of
    T.Slash -> recurse S.Slash
    T.Star  -> recurse S.Star
    _       -> Right (expr, t :| ts)
   where
    recurse op = unary (fromList ts) >>= factor' . first (Binary (op, snd t) expr)

unary :: Parser Expr
unary (t :| ts) = case fst t of
  T.Bang  -> (first . Unary) (S.Bang, snd t) <$> unary (fromList ts)
  T.Minus -> (first . Unary) (S.Minus', snd t) <$> unary (fromList ts)
  _       -> call (t :| ts)

call :: Parser Expr
call tokens@(h :| _) = primary tokens >>= call'
 where
  call' (expr, t :| ts) = case fst t of
    T.LeftParen -> finish expr (fromList ts) >>= call'
    T.Dot -> case fromList ts of
      (T.Identifier prop, pos) :| ts' -> call' (Get expr (prop, pos), fromList ts')
      ts'                             -> Left $ ParseError "Expected prop/method name after '.'" $ head ts'
    _ -> Right (expr, t :| ts)

  finish expr tokens' = do
    (args, t' :| ts') <- arguments tokens'
    when (length args >= 255) $ Left $ ParseError ">= 255 args" t'
    case fst t' of
      T.RightParen -> Right (Call (expr, snd h) args, fromList ts')
      _            -> Left $ ParseError "Expected ')' after args" t'

  arguments :: Parser [Expr]
  arguments tokens'@((fst -> T.RightParen) :| _) = Right ([], tokens')
  arguments tokens' = do
    (arg, t' :| ts') <- expression tokens'
    case fst t' of
      T.Comma -> first (arg :) <$> arguments (fromList ts')
      _       -> Right ([arg], t' :| ts')

primary :: Parser Expr
primary (t :| ts) = let ts' = fromList ts in case fst t of
  T.False'       -> Right (Literal $ Bool' False, ts')
  T.True'        -> Right (Literal $ Bool' True, ts')
  T.Nil          -> Right (Literal S.Nil, ts')
  T.Number' n    -> Right (Literal $ S.Number' n, ts')
  T.String' s    -> Right (Literal $ S.String' s, ts')
  T.Identifier i -> Right (Variable (i, snd t), ts')
  T.This         -> Right (S.This (snd t), ts')
  T.Super        -> superExpr (t :| ts)
  T.LeftParen    -> grouping (t :| ts)
  _              -> Left $ ParseError "Expected expr" t

superExpr :: Parser Expr
superExpr (t :| ts) = case fromList ts of
  (fst -> T.Dot) :| (T.Identifier name, pos) : rest -> Right (S.Super (snd t) (name, pos), fromList rest)
  (fst -> T.Dot) :| rest -> Left $ ParseError "Expected superclass method name after '.'" $ head $ fromList rest
  ts' -> Left $ ParseError "Expected '.' after 'super'" $ head ts'

grouping :: Parser Expr
grouping (t :| ts) = do
  (expr, t' :| ts') <- expression $ fromList ts
  case fst t' of
    T.RightParen -> Right (Grouping expr, fromList ts')
    _            -> Left $ ParseError "Expected ')' after expr" t
