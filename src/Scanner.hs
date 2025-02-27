module Scanner where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Token

data ScanError = ScanError
  { message :: String
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
      let validChars v = isAlphaNum v || v == '_'
      let (lexeme, cs') = span validChars chars
      let token = (scanWord lexeme, line)
      (token :) <$> scanTokens cs' line

  -- Numbers
  | isDigit c = do
      let (tokenType, cs') = scanNumber chars
      let token = (tokenType, line)
      (token :) <$> scanTokens cs' line

  -- Strings
  | c == '"' = do
      let (lexeme, cs') = span (/= '"') cs
      case cs' of
        _ : cs'' ->
          let token = (String' lexeme, line)
           in (token :) <$> scanTokens cs'' line
        _ ->
          Left $
            ScanError "Unterminated string" line $
              takeWhile (/= '\n') lexeme

  -- Whitespaces
  | c `elem` [' ', '\t', '\r'] = scanTokens cs line
  -- Newline
  | c == '\n' = scanTokens cs (line + 1)
  --
  -- Division and comments
  | c == '/' =
      case cs of
        '/' : cs' ->
          let cs'' = dropWhile (/= '\n') cs'
           in scanTokens cs'' line
        _ ->
          let token = (Slash, line)
           in (token :) <$> scanTokens cs line
  -- Single and double char tokens
  | otherwise =
      case chars of
        c' : c'' : cs'
          | Just tokenType <- scanDoubleChar c' c'' ->
              let token = (tokenType, line)
               in (token :) <$> scanTokens cs' line
        _c : _cs
          | Just tokenType <- scanSingleChar c ->
              let token = (tokenType, line)
               in (token :) <$> scanTokens cs line
        _ -> Left (ScanError "Unidentified token" line [c])

scanWord :: String -> TokenType
scanWord lexeme = case lexeme of
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

scanNumber :: String -> (TokenType, String)
scanNumber chars =
  let (intPart, cs) = span isDigit chars
      (numberStr, cs') = case cs of
        ('.' : rest) ->
          let (decPart, cs'') = span isDigit rest
           in (intPart ++ "." ++ decPart, cs'')
        _ -> (intPart, cs)
   in (Number' (read numberStr), cs')

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
