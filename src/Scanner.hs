module Scanner where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Token

data ScanError = ScanError
  { error :: String
  , line :: Int
  , lexeme :: String
  }
  deriving (Show)

scan :: String -> Either ScanError [Token]
scan source = scanTokens source 1

scanTokens :: String -> Int -> Either ScanError [Token]
scanTokens [] line = Right [(Eof, line)]
scanTokens chars@(c : cs) line
  -- Keywords and identifiers
  | isAlpha c || c == '_' = do
      let validChars c' = isAlphaNum c' || c' == '_'
      let (lexeme, cs') = span validChars chars
      let token = (scanLexeme lexeme, line)
      (token :) <$> scanTokens cs' line

  -- Numbers
  | isDigit c = do
      let validChars c' = isDigit c' || c' == '.'
      let (lexeme, cs') = span validChars chars
      if (length . filter (== '.') $ lexeme) > 1
        then Left (ScanError "Invalid number format" line lexeme)
        else do
          let token = (Number' . read $ lexeme, line)
          (token :) <$> scanTokens cs' line

  -- Strings
  | c == '"' = do
      let (lexeme, cs') = span (/= '"') cs
      if null cs'
        then Left $ ScanError "Unterminated string" line $ takeWhile (/= '\n') lexeme
        else do
          let token = (String' lexeme, line)
          (token :) <$> scanTokens (drop 1 cs') line

  -- Whitespaces
  | c `elem` [' ', '\t', '\r'] = scanTokens cs line
  -- Newline
  | c == '\n' = scanTokens cs (line + 1)
  -- Division and comments
  | c == '/' =
      case cs of
        '/' : cs' -> scanTokens (dropWhile (/= '\n') cs') line
        _ -> ((Slash, line) :) <$> scanTokens cs line
  -- Single and double char tokens
  | otherwise =
      case scanSymbol chars of
        Nothing -> Left (ScanError "Unidentified token" line [c])
        Just (tokenType, cs') -> ((tokenType, line) :) <$> scanTokens cs' line

scanLexeme :: String -> TokenType
scanLexeme lexeme = case lexeme of
  "and" -> And
  "class" -> Class
  "else" -> Else
  "false" -> False'
  "fun" -> Fun
  "for" -> For
  "if" -> If
  "nil" -> Nil
  "or" -> Or
  "print" -> Print
  "return" -> Return
  "super" -> Super
  "this" -> This
  "true" -> True'
  "var" -> Var
  "while" -> While
  _ -> Identifier lexeme

scanSymbol :: String -> Maybe (TokenType, String)
scanSymbol (c0 : c1 : cs) | Just tokenType <- scanDoubleChar c0 c1 = Just (tokenType, cs)
scanSymbol (c : cs) | Just tokenType <- scanSingleChar c = Just (tokenType, cs)
scanSymbol _ = Nothing

scanDoubleChar :: Char -> Char -> Maybe TokenType
scanDoubleChar c0 c1 = case [c0, c1] of
  "!=" -> Just BangEqual
  "==" -> Just EqualEqual
  "<=" -> Just LessEqual
  ">=" -> Just GreaterEqual
  _ -> Nothing

scanSingleChar :: Char -> Maybe TokenType
scanSingleChar c = case c of
  '(' -> Just LeftParen
  ')' -> Just RightParen
  '{' -> Just LeftBrace
  '}' -> Just RightBrace
  ',' -> Just Comma
  '.' -> Just Dot
  '-' -> Just Minus
  '+' -> Just Plus
  ';' -> Just Semicolon
  '*' -> Just Star
  '!' -> Just Bang
  '=' -> Just Equal
  '>' -> Just Greater
  '<' -> Just Less
  _ -> Nothing
