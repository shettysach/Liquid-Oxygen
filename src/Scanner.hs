module Scanner where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Token

scan :: String -> Either String [Token]
scan source = scanTokens source 1

scanTokens :: String -> Int -> Either String [Token]
scanTokens [] line = Right [(Eof, line)]
scanTokens source@(curr : rest) line
  -- Keywords and identifiers
  | isAlpha curr || curr == '_' = do
      let (lexeme, rest') = span (\c -> isAlphaNum c || c == '_') source
      ((scanLexeme lexeme, line) :) <$> scanTokens rest' line
  -- Numbers
  | isDigit curr = do
      let (lexeme, rest') = span (\c -> isDigit c || c == '.') source
      if (length . filter (== '.') $ lexeme) > 1
        then Left $ "Invalid number format at line " ++ show line ++ " - " ++ lexeme
        else ((NumberToken . read $ lexeme, line) :) <$> scanTokens rest' line
  -- Whitespaces
  | curr `elem` [' ', '\t', '\r'] = scanTokens rest line
  -- Newline
  | curr == '\n' = scanTokens rest (line + 1)
  -- Strings
  | curr == '"' = do
      let (lexeme, rest') = span (/= '"') rest
      if null rest'
        then Left $ "Unterminated string at line " ++ show line ++ " - " ++ takeWhile (/= '\n') lexeme
        else ((StringToken lexeme, line) :) <$> scanTokens (tail rest') line
  -- Division and comments
  | curr == '/' =
      case rest of
        '/' : rest' -> scanTokens (dropWhile (/= '\n') rest') line
        _ -> ((Slash, line) :) <$> scanTokens rest line
  -- Symbols
  | otherwise =
      let lexeme = scanSymbol source
       in case lexeme of
            Nothing -> Left $ "Unidentified token " ++ show curr ++ " at line " ++ show line
            Just (symbol, rest') -> ((symbol, line) :) <$> scanTokens rest' line

scanLexeme :: String -> TokenType
scanLexeme lexeme = case lexeme of
  "and" -> And
  "class" -> Class
  "else" -> Else
  "false" -> FalseToken
  "fun" -> Fun
  "for" -> For
  "if" -> If
  "nil" -> Nil
  "or" -> Or
  "print" -> Print
  "return" -> Return
  "super" -> Super
  "this" -> This
  "true" -> TrueToken
  "var" -> Var
  "while" -> While
  _ -> Identifier lexeme

scanSymbol :: String -> Maybe (TokenType, String)
scanSymbol (c0 : c1 : rest) | Just tokenType <- scanDoubleChar c0 c1 = Just (tokenType, rest)
scanSymbol (c : rest) | Just tokenType <- scanSingleChar c = Just (tokenType, rest)
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
