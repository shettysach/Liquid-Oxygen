module Scanner where

import Data.Char          (isAlpha, isAlphaNum, isDigit)
import Data.List.NonEmpty (NonEmpty, fromList, (<|))

import Error              (ScanError (ScanError))
import Token

scan :: String -> Either ScanError (NonEmpty Token)
scan source = scan' source (1, 1)

scan' :: String -> (Int, Int) -> Either ScanError (NonEmpty Token)
scan' [] pos = Right $ fromList [(Eof, pos)]
scan' chars@(c : cs) pos@(line, col)
  -- Keywords and identifiers
  | isAlpha c || c == '_' =
      let (lexeme, cs') = span (\x -> isAlphaNum x || x == '_') chars
          token = (scanWord lexeme, pos)
          col' = col + length lexeme
       in (token <|) <$> scan' cs' (line, col')
  -- Numbers
  | isDigit c =
      let (lexeme, cs') = scanNumber chars
          token = (Number' (read lexeme), pos)
          col' = col + length lexeme
       in (token <|) <$> scan' cs' (line, col')
  -- Whitespaces
  | c `elem` [' ', '\t', '\r'] = scan' cs (line, col + 1)
  -- Newline
  | c == '\n' = scan' cs (line + 1, 1)
  -- Strings
  | c == '"' =
      let (lexeme, cs1) = span (/= '"') cs
       in case cs1 of
            '"' : cs2 ->
              let token = (String' lexeme, pos)
                  line' = line + length (filter (== '\n') lexeme)
                  col' = col + length lexeme + 2
               in (token <|) <$> scan' cs2 (line', col')
            _ -> Left $ ScanError "Unterminated string" (lexeme, pos)
  -- Divison and comments
  | c == '/' = case cs of
      '/' : cs' -> scan' (dropWhile (/= '\n') cs') (line, 1)
      _         -> ((Slash, pos) <|) <$> scan' cs (line, col + 1)
  -- Single and double char tokens
  | otherwise = case cs of
      c' : cs'
        | Just tokenType <- scanDoubleChar c c' ->
            let token = (tokenType, pos)
             in (token <|) <$> scan' cs' (line, col + 2)
      _
        | Just tokenType <- scanSingleChar c ->
            let token = (tokenType, pos)
             in (token <|) <$> scan' cs (line, col + 1)
        | otherwise -> Left $ ScanError "Unidentified token" (show c, pos)

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
  let (intPart, afterInt) = span isDigit chars
   in case afterInt of
        '.' : d : ds
          | isDigit d ->
              let (decPart, afterDec) = span isDigit (d : ds)
               in (intPart ++ "." ++ decPart, afterDec)
        _ -> (intPart, afterInt)

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
