module Scanner where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Error     (ScanError (ScanError))
import Token

scan :: String -> Either ScanError [Token]
scan source = scanTokens source (1, 1)

scanTokens :: String -> (Int, Int) -> Either ScanError [Token]
scanTokens [] pos = Right [(Eof, pos)]
scanTokens chars@(c : _) pos@(line, col)
  -- Keywords and identifiers
  | isAlpha c || c == '_' =
      let (lexeme, cs') = span (\x -> isAlphaNum x || x == '_') chars
          token = (scanWord lexeme, pos)
          col' = col + length lexeme
       in (token :) <$> scanTokens cs' (line, col')
  -- Numbers
  | isDigit c =
      let (lexeme, cs') = scanNumber chars
          token = (Number' (read lexeme), pos)
          col' = col + length lexeme
       in (token :) <$> scanTokens cs' (line, col')
scanTokens (c : cs) pos@(line, col) =
  case c of
    -- Strings
    '"' ->
      let (lexeme, cs1) = span (/= '"') cs
       in case cs1 of
            '"' : cs2 ->
              let token = (String' lexeme, pos)
                  line' = line + length (filter (== '\n') lexeme)
                  col' = col + length lexeme + 2
               in (token :) <$> scanTokens cs2 (line', col')
            _ -> Left $ ScanError "Unterminated string" lexeme pos
    -- Whitespaces and newline
    ' ' -> scanTokens cs (line, col + 1)
    '\t' -> scanTokens cs (line, col + 1)
    '\r' -> scanTokens cs (line, col + 1)
    '\n' -> scanTokens cs (line + 1, 1)
    -- Divison and comments
    '/' -> case cs of
      '/' : cs' -> scanTokens (dropWhile (/= '\n') cs') (line, 1)
      _         -> ((Slash, pos) :) <$> scanTokens cs (line, col + 1)
    -- Single and double char tokens
    _ -> case cs of
      c' : cs'
        | Just tokenType <- scanDoubleChar c c' ->
            let token = (tokenType, pos)
             in (token :) <$> scanTokens cs' (line, col + 2)
      cs'
        | Just tokenType <- scanSingleChar c ->
            let token = (tokenType, pos)
             in (token :) <$> scanTokens cs' (line, col + 1)
      _ -> Left (ScanError "Unidentified token" [c] pos)

scanWord :: String -> TokenType
scanWord lexeme = case lexeme of
  "and"    -> And
  "class"  -> Class
  "else"   -> Else
  "false"  -> False'
  "fun"    -> Fun
  "for"    -> For
  "if"     -> If
  "nil"    -> Nil
  "or"     -> Or
  "print"  -> Print
  "return" -> Return
  "super"  -> Super
  "this"   -> This
  "true"   -> True'
  "var"    -> Var
  "while"  -> While
  _        -> Identifier lexeme

scanNumber :: String -> (String, String)
scanNumber chars =
  let (intPart, cs) = span isDigit chars
   in case cs of
        ('.' : cs1) ->
          let (decPart, cs2) = span isDigit cs1
           in (intPart ++ "." ++ decPart, cs2)
        _ -> (intPart, cs)

scanDoubleChar :: Char -> Char -> Maybe TokenType
scanDoubleChar c0 c1 = case [c0, c1] of
  "!=" -> Just BangEqual
  "==" -> Just EqualEqual
  "<=" -> Just LessEqual
  ">=" -> Just GreaterEqual
  _    -> Nothing

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
  _   -> Nothing
