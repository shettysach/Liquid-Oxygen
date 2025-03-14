module Parser where

import Control.Arrow (first)
import Control.Monad (when)
import Data.Functor  ((<&>))

import AST           as A
import Token         as T

parse :: [Token] -> Either ParseError [Stmt]
parse = parse' []
 where
  parse' stmts [token] | fst token == T.Eof = Right (reverse stmts)
  parse' stmts tokens = statement tokens >>= uncurry (parse' . (: stmts))

type Parse a = [Token] -> Either ParseError (a, [Token])

statement :: Parse Stmt
statement [] = undefined
statement (t : ts) =
  case fst t of
    T.Var       -> declaration ts
    T.Print     -> expression ts >>= uncurry (statement' . A.Print)
    T.LeftBrace -> block [] ts <&> first A.Block
    T.If        -> ifStatement ts
    T.While     -> while ts
    T.For       -> for ts
    _           -> expression (t : ts) >>= uncurry (statement' . A.Expr)

statement' :: Stmt -> Parse Stmt
statement' stmt (t : ts) | fst t == T.Semicolon = Right (stmt, ts)
statement' _ tokens = Left $ ParseError "Expected ';'" (head tokens)

block :: [Stmt] -> Parse [Stmt]
block stmts (t : ts) | fst t == T.RightBrace = Right (reverse stmts, ts)
block stmts tokens = statement tokens >>= uncurry (block . (: stmts))

declaration :: Parse Stmt
declaration ((T.Identifier name, _) : rest) = case rest of
  t : ts | fst t == T.Equal    -> expression ts >>= uncurry (statement' . A.Var name)
  t : _ | fst t == T.Semicolon -> statement' (A.Var name $ Literal A.Nil) rest
  _                            -> Left $ ParseError "Expected = after var name" (head rest)
declaration tokens = Left $ ParseError "Expected var name" (head tokens)

ifStatement :: Parse Stmt
ifStatement (t : ts) | fst t == T.LeftParen = do
  (condition, afterCondn) <- expression ts
  (thenBranch, afterThen) <- case afterCondn of
    t' : ts' | fst t' == T.RightParen -> statement ts'
    _ -> Left $ ParseError "Expected ')' after if condition" (head afterCondn)
  (elseBranch, afterElse) <- case afterThen of
    t' : ts' | fst t' == T.Else -> statement ts' <&> first Just
    _                           -> Right (Nothing, afterThen)
  Right (A.If condition thenBranch elseBranch, afterElse)
ifStatement tokens = Left $ ParseError "Expected '(' after 'if'" (head tokens)

while :: Parse Stmt
while (t : ts) | fst t == T.LeftParen = do
  (condition, afterCondn) <- expression ts
  case afterCondn of
    t' : ts' | fst t' == T.RightParen -> statement ts' <&> first (A.While condition)
    _ -> Left $ ParseError "Expected ')' after while condition" (head ts)
while tokens = Left $ ParseError "Expected '(' after 'while'" (head tokens)

for :: Parse Stmt
for (t : ts) | fst t == T.LeftParen = do
  (initializer, afterInit) <- case ts of
    t1 : ts1
      | fst t1 == T.Semicolon -> return (Nothing, ts1)
      | fst t1 == T.Var -> declaration ts1 <&> first Just
    _ -> do
      (expr, after) <- expression ts
      case after of
        t2 : ts2 | fst t2 == T.Semicolon -> return (Just (Expr expr), ts2)
        _ -> Left $ ParseError "Expected ';' after loop initializer" (head after)

  (condition, afterCondn) <- case afterInit of
    t1 : ts1 | fst t1 == T.Semicolon -> return (Literal (Bool' True), ts1)
    _ -> do
      (expr, after) <- expression afterInit
      case after of
        t2 : ts2 | fst t2 == T.Semicolon -> return (expr, ts2)
        _ -> Left $ ParseError "Expected ';' after loop condition" (head after)

  (increment, afterInc) <- case afterCondn of
    t1 : ts1 | fst t1 == T.RightParen -> return (Nothing, ts1)
    _ -> do
      (expr, after) <- expression afterCondn
      case after of
        t2 : ts2 | fst t2 == T.RightParen -> return (Just expr, ts2)
        _ -> Left $ ParseError "Expected ')' after increment" (head after)

  (stmt, afterStmt) <- statement afterInc

  let body = case increment of
        Nothing  -> stmt
        Just inc -> Block [stmt, Expr inc]
      stmt' = A.While condition body
      loop = case initializer of
        Nothing    -> stmt'
        Just init' -> Block [init', stmt']
  Right (loop, afterStmt)
for tokens = Left $ ParseError "Expected '(' after 'for'" (head tokens)

-- Expr

expression :: Parse Expr
expression = assignment

assignment :: Parse Expr
assignment tokens =
  Parser.or tokens >>= uncurry assignment'
 where
  assignment' expr (t : ts)
    | fst t == T.Equal =
        assignment ts >>= \(value, ts') ->
          case expr of
            Variable var -> Right (A.Assignment var value, ts')
            _            -> Left $ ParseError "Invalid target" t
  assignment' expr ts = Right (expr, ts)

-- Logical

or :: Parse Expr
or tokens =
  Parser.and tokens >>= uncurry or'
 where
  or' expr [] = Right (expr, [])
  or' expr (t : ts) =
    case fst t of
      T.Or -> recurse (A.Or, snd t)
      _    -> Right (expr, t : ts)
   where
    recurse op = comparison ts >>= uncurry (or' . Logical op expr)

and :: Parse Expr
and tokens =
  equality tokens >>= uncurry and'
 where
  and' expr [] = Right (expr, [])
  and' expr (t : ts) =
    case fst t of
      T.And -> recurse A.And
      _     -> Right (expr, t : ts)
   where
    recurse op = comparison ts >>= uncurry (and' . Logical (op, snd t) expr)

-- Binary

equality :: Parse Expr
equality tokens =
  comparison tokens >>= uncurry equality'
 where
  equality' expr [] = Right (expr, [])
  equality' expr (t : ts) =
    case fst t of
      T.BangEqual  -> recurse A.BangEqual
      T.EqualEqual -> recurse A.EqualEqual
      _            -> Right (expr, t : ts)
   where
    recurse op = comparison ts >>= uncurry (equality' . Binary (op, snd t) expr)

comparison :: Parse Expr
comparison tokens =
  term tokens >>= uncurry comparison'
 where
  comparison' expr [] = Right (expr, [])
  comparison' expr (t : ts) =
    case fst t of
      T.Greater      -> recurse A.Greater
      T.GreaterEqual -> recurse A.GreaterEqual
      T.Less         -> recurse A.Less
      T.LessEqual    -> recurse A.LessEqual
      _              -> Right (expr, t : ts)
   where
    recurse op = term ts >>= uncurry (comparison' . Binary (op, snd t) expr)

term :: Parse Expr
term tokens =
  factor tokens >>= uncurry term'
 where
  term' expr [] = Right (expr, [])
  term' expr (t : ts) =
    case fst t of
      T.Minus -> recurse A.Minus
      T.Plus  -> recurse A.Plus
      _       -> Right (expr, t : ts)
   where
    recurse op = factor ts >>= uncurry (term' . Binary (op, snd t) expr)

factor :: Parse Expr
factor tokens =
  unary tokens >>= uncurry factor'
 where
  factor' expr [] = Right (expr, [])
  factor' expr (t : ts) =
    case fst t of
      T.Slash -> recurse A.Slash
      T.Star  -> recurse A.Star
      _       -> Right (expr, t : ts)
   where
    recurse op = unary ts >>= uncurry (factor' . Binary (op, snd t) expr)

-- Unary

unary :: Parse Expr
unary [] = call []
unary (t : ts) =
  case fst t of
    T.Bang  -> unary ts <&> (first . Unary) (A.Bang, snd t)
    T.Minus -> unary ts <&> (first . Unary) (A.Minus', snd t)
    _       -> call (t : ts)

-- Function

call :: Parse Expr
call tokens =
  primary tokens >>= uncurry call'
 where
  call' expr [] = Right (expr, [])
  call' expr (t : ts) =
    case fst t of
      T.LeftParen -> finish expr ts >>= uncurry call'
      _           -> Right (expr, t : ts)

finish :: Expr -> Parse Expr
finish callee tokens = do
  (args, rest) <- arguments tokens
  when
    (length args >= 255)
    (Left $ ParseError ">= 255 args" $ head rest)
  case rest of
    (RightParen, _) : ts -> Right (Call callee args, ts)
    _                    -> Left $ ParseError "Expected ')' after arguments" (head rest)

arguments :: Parse [Expr]
arguments (t : ts) | fst t == T.RightParen = Right ([], t : ts)
arguments tokens = do
  (arg, rest) <- expression tokens
  case rest of
    (t' : ts') | fst t' == T.Comma -> arguments ts' <&> first (arg :)
    _                              -> Right ([arg], rest)

-- Primary

primary :: Parse Expr
primary [] = undefined
primary (t : ts) = case fst t of
  T.False' -> Right (Literal $ Bool' False, ts)
  T.True' -> Right (Literal $ Bool' True, ts)
  T.Nil -> Right (Literal A.Nil, ts)
  T.Number' n -> Right (Literal $ A.Number' n, ts)
  T.String' s -> Right (Literal $ A.String' s, ts)
  T.Identifier i -> Right (Variable (i, snd t), ts)
  T.LeftParen -> do
    (expr, rest) <- expression ts
    case rest of
      t' : ts' | fst t' == T.RightParen -> Right (Grouping expr, ts')
      _                                 -> Left $ ParseError "Expected ')' after expr" t
  _ -> Left $ ParseError "Expected expr" t

-- Error

data ParseError = ParseError String Token

instance Show ParseError where
  show (ParseError message (token, position)) =
    "\n\ESC[31m"
      ++ "Parse Error - "
      ++ "\ESC[0m"
      ++ message
      ++ "\nTokenType   - "
      ++ show token
      ++ "\nPosition    - "
      ++ show position
