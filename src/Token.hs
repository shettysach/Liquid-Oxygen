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
  | -- One or two character tokens
    Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | -- Variables and literals
    Identifier String
  | Number' Double
  | String' String
  | -- Keywords
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
  deriving (Eq)

instance Show TokenType where
  show LeftParen      = "("
  show RightParen     = ")"
  show LeftBrace      = "{"
  show RightBrace     = "}"
  show Comma          = ","
  show Dot            = "."
  show Minus          = "-"
  show Plus           = "+"
  show Semicolon      = ";"
  show Slash          = "/"
  show Star           = "*"
  show Bang           = "!"
  show BangEqual      = "!="
  show Equal          = "="
  show EqualEqual     = "=="
  show Greater        = ">"
  show GreaterEqual   = ">="
  show Less           = "<"
  show LessEqual      = "<="
  show (Identifier s) = s
  show (Number' n)    = show n
  show (String' s)    = show s
  show And            = "and"
  show Class          = "class"
  show Else           = "else"
  show False'         = "false"
  show Fun            = "fun"
  show For            = "for"
  show If             = "if"
  show Nil            = "nil"
  show Or             = "or"
  show Print          = "print"
  show Return         = "return"
  show Super          = "super"
  show This           = "this"
  show True'          = "true"
  show Var            = "var"
  show While          = "while"
  show Eof            = "EOF"
