module Scanner where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Token

scan :: String -> [Token]
scan source = scanTokens source 1

scanTokens :: String -> Int -> [Token]
scanTokens (c : cs) line
  | isAlpha c =
      let (lexeme, rest) = span isAlphaNum (c : cs)
          tokenType = scanLexeme lexeme
       in (tokenType, line) : scanTokens rest line
  | isDigit c =
      let (lexeme, rest) = span isDigit (c : cs)
          value = read lexeme :: Double
       in (NumberToken value, line) : scanTokens rest line
  | c == '\n' = scanTokens cs (line + 1)
  | isSpace c = scanTokens cs line
  | c == '"' =
      let (lexeme, rest) = span (/= '"') cs
       in if null rest
            then error $ "Unterminated string at line " ++ show line
            else (StringToken lexeme, line) : scanTokens (tail rest) line
  | otherwise =
      let (tokenType, rest) = scanSymbol (c : cs)
       in (tokenType, line) : scanTokens rest line
scanTokens _ line = [(Eof, line)]

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

scanSymbol :: String -> (TokenType, String)
scanSymbol (c0 : c1 : cs) | Just tokenType <- scanDoubleChar c0 c1 = (tokenType, cs)
scanSymbol (c : cs) | Just tokenType <- scanSingleChar c = (tokenType, cs)
scanSymbol _ = error "Unexpected end of input"

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
