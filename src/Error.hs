module Error where

import Token

data ScanError = ScanError String String (Int, Int)

data ParseError = ParseError String Token

data ResolveError = ResolveError String String (Int, Int)

data RuntimeError = RuntimeError String String (Int, Int)

instance Show ScanError where
  show (ScanError message lexeme position) =
    "\nSCAN ERROR : "
      ++ message
      ++ "\nLexeme     : "
      ++ lexeme
      ++ "\nPosition   : "
      ++ show position

instance Show ParseError where
  show (ParseError message (token, position)) =
    "\nPARSE ERROR : "
      ++ message
      ++ "\nTokenType   : "
      ++ show token
      ++ "\nPosition    : "
      ++ show position

instance Show ResolveError where
  show (ResolveError message lexeme position) =
    "\nRESOLVE ERROR : "
      ++ message
      ++ "\nLexeme        : "
      ++ lexeme
      ++ "\nPosition      : "
      ++ show position

instance Show RuntimeError where
  show (RuntimeError message lexeme position) =
    "\nRUNTIME ERROR : "
      ++ message
      ++ "\nLexeme        : "
      ++ lexeme
      ++ "\nPosition      : "
      ++ show position
