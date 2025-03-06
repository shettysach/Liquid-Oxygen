module Token where

type Token = (TokenType, (Int, Int))

data TokenType
  = -- Single-character tokens.
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | -- One or two character tokens.
    Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | -- Literals.
    Identifier String
  | Number' Double
  | String' String
  | -- Keywords.
    And
  | Class
  | Else
  | False'
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True'
  | Var
  | While
  | Eof
  deriving (Show, Eq)
