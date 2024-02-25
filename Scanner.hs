module Scanner(
  scanTokens 
) where

import Types (Token (..), TokenType (..))
import Data.Char
import Data.Maybe

scanTokens :: String -> [Token]
scanTokens = scanTokens' 1

scanTokens' :: Int -> String -> [Token]
scanTokens' line [] = [createToken EOF [] line Nothing]
scanTokens' line [x] = if isSpace x then scanTokens' line [] else charToken line x : scanTokens' line []
scanTokens' line code@(x : y : ys) = case x of
  '!' -> if y == '=' then add2CharToken BANG_EQUAL else add1CharToken
  '=' -> if y == '=' then add2CharToken EQUAL_EQUAL else add1CharToken
  '<' -> if y == '=' then add2CharToken LESS_EQUAL else add1CharToken
  '>' -> if y == '=' then add2CharToken GREATER_EQUAL else add1CharToken
  '/' -> if y == '/' then (scanTokens' line . consumeComment) ys else add1CharToken
  '"' -> addString (y : ys)
  _
    | isSpace x && x /= '\n' -> scanTokens' line (y : ys)
    | x == '\n' -> scanTokens' (line + 1) (y : ys)
    | isDigit x -> addNumber code
    | isAlphaOrUnderScore x || x == '_' -> addIdentifier code
    | otherwise -> add1CharToken
  where
    add2CharToken tok = createToken tok [x, y] line Nothing : scanTokens' line ys
    add1CharToken = charToken line x : scanTokens' line (y : ys)
    consumeComment = dropWhile (/= '\n')

    addString code = if null rest
        then error ("Line " ++ show line ++ ": unterminated string at line")
        else createToken STRING str line Nothing : scanTokens' (line + length (filter (=='\n') str)) (tail rest)
      where
        (str, rest) = span (/= '"') code

    addNumber code = createToken NUMBER strNum line (Just num) : scanTokens' line rest
      where
        (preDecimal, other) = span isDigit code
        (postDecimal, rest) = case other of
          [] -> ([], [])
          (x:xs) -> if x == '.' then span isDigit xs else ([], other)
        strNum = case postDecimal of
          [] -> preDecimal
          _ -> preDecimal ++ "." ++ postDecimal
        num = stringToDouble strNum

    isAlphaOrUnderScore x = isAlpha x || x == '_'
    addIdentifier code = createToken (fromMaybe IDENTIFIER (keywordType iden)) iden line Nothing : scanTokens' line rest
      where
        (iden, rest) = span (\x -> isAlphaOrUnderScore x || isDigit x) code
        keywordType str = case str of
          "and" -> Just AND
          "class" -> Just CLASS
          "print" -> Just PRINT
          "or" -> Just OR
          "if" -> Just IF
          "else" -> Just ELSE
          "while" -> Just WHILE
          "for" -> Just FOR
          "fun" -> Just FUN
          "super" -> Just SUPER
          "this" -> Just THIS
          "var" -> Just VAR
          "return" -> Just RETURN
          "true" -> Just TRUE
          "false" -> Just FALSE
          "nil" -> Just NIL
          _ -> Nothing

createToken :: TokenType -> String -> Int -> Maybe Double -> Token
createToken tok lex lineNumber num = Token {tokenType = tok, lexeme = lex, number = num, line = lineNumber}

charToken :: Int -> Char -> Token
charToken line x = case x of
  '(' -> create LEFT_PAREN
  ')' -> create RIGHT_PAREN
  '{' -> create LEFT_BRACE
  '}' -> create RIGHT_BRACE
  ',' -> create COMMA
  '.' -> create DOT
  '-' -> create MINUS
  '+' -> create PLUS
  ';' -> create SEMICOLON
  '*' -> create STAR
  '!' -> create BANG
  '=' -> create EQUAL
  '<' -> create LESS
  '>' -> create GREATER
  '/' -> create SLASH
  _ 
    | isDigit x -> createToken NUMBER [x] line (Just $ stringToDouble [x])
    | otherwise -> error $ "Line " ++ show line ++ ": character `" ++ [x] ++ "` not recognized"
  where
    create tok = createToken tok [x] line Nothing

stringToDouble x = read x :: Double