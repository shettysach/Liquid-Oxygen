module Token where

type IdentifierLiteral = String

type StringLiteral = String

type NumberLiteral = Double

type Token = (TokenType, Int)

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
    Identifier IdentifierLiteral
  | NumberToken NumberLiteral
  | StringToken StringLiteral
  | -- Keywords.
    And
  | Class
  | Else
  | FalseToken
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | TrueToken
  | Var
  | While
  | Eof
  deriving (Show, Eq)
