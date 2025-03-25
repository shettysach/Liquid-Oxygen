module Error where

import Token

data ScanError = ScanError String String (Int, Int)

data ParseError = ParseError String Token

data RuntimeError = RuntimeError String String (Int, Int)

instance Show ScanError where
  show (ScanError message lexeme position) =
    "\n\ESC[31m"
      ++ "Scan Error - "
      ++ "\ESC[0m"
      ++ message
      ++ "\nLexeme     - "
      ++ lexeme
      ++ "\nPosition   - "
      ++ show position

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

instance Show RuntimeError where
  show (RuntimeError message lexeme position) =
    "\n\ESC[31m"
      ++ "Runtime Error - "
      ++ "\ESC[0m"
      ++ message
      ++ "\nLexeme        - "
      ++ lexeme
      ++ "\nPosition      - "
      ++ show position
