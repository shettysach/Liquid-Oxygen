module Scanner where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Token

scan :: String -> Either String [Token]
scan source = scanTokens source 1

scanTokens :: String -> Int -> Either String [Token]
scanTokens [] line = Right [(Eof, line)]
scanTokens (c : cs) line
  | isAlpha c = do
      let (lexeme, rest) = span isAlphaNum (c : cs)
          tokenType = scanLexeme lexeme
      ((tokenType, line) :) <$> scanTokens rest line
  | isDigit c = do
      let (lexeme, rest) = span isDigit (c : cs)
          value = read lexeme :: Double
      ((NumberToken value, line) :) <$> scanTokens rest line
  | c == '\n' = scanTokens cs (line + 1)
  | isSpace c = scanTokens cs line
  | c == '"' =
      let (lexeme, rest) = span (/= '"') cs
       in if null rest
            then Left $ "Unterminated string at line " ++ show line
            else ((StringToken lexeme, line) :) <$> scanTokens (tail rest) line
  | otherwise =
      let symbol = scanSymbol (c : cs)
       in case symbol of
            Just (tokenType, rest) -> ((tokenType, line) :) <$> scanTokens rest line
            Nothing -> Left $ " Unidentified token at line " ++ show line

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
  '/' -> Just Slash
  '*' -> Just Star
  '!' -> Just Bang
  '=' -> Just Equal
  '>' -> Just Greater
  '<' -> Just Less
  _ -> Nothing
