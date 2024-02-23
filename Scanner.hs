module Scanner where

import Types (Token (..), TokenType (..))
import Data.Char
import Data.Maybe

scanTokens :: String -> [Token]
scanTokens [] = [createToken EOF [] 0 Nothing]
scanTokens [x] = if isSpace x then scanTokens [] else charToken x : scanTokens []
scanTokens code@(x : y : ys) = case x of
  '!' -> if y == '=' then char2Token BANG_EQUAL else charTok
  '=' -> if y == '=' then char2Token EQUAL_EQUAL else charTok
  '<' -> if y == '=' then char2Token LESS_EQUAL else charTok
  '>' -> if y == '=' then char2Token GREATER_EQUAL else charTok
  '/' -> if y == '/' then (scanTokens . consumeComment) ys else charTok
  '"' -> addString (y : ys)
  _
    | isSpace x -> scanTokens (y : ys)
    | isDigit x -> addNumber code
    | isAlphaOrUnderScore x || x == '_' -> addIdentifier code
    | otherwise -> charTok
  where
    char2Token tok = createNonLiteralToken tok [x, y] : scanTokens ys
    charTok = charToken x : scanTokens (y : ys)
    consumeComment = dropWhile (/= '\n')

    addString code = createToken STRING str 0 Nothing : scanTokens rest
      where
        (str, _ : rest) = span (/= '"') code

    addNumber code = createToken NUMBER strNum 0 (Just num) : scanTokens rest
      where
        (preDecimal, other) = span isDigit code
        (postDecimal, rest) =
          if null other -- file ends with number
            then ([], other)
            else -- parse out decimal part of number

              let (digits, r) = span isDigit (tail other)
               in if null digits -- no decimal components
                    then ([], '.' : r)
                    else ('.' : digits, r)
        strNum = preDecimal ++ postDecimal
        num = stringToDouble strNum

    isAlphaOrUnderScore x = isAlpha x || x == '_'
    addIdentifier code = createToken (fromMaybe IDENTIFIER (keywordType iden)) iden 0 Nothing : scanTokens rest
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

createToken tok lex lineNumber num = Token {tokenType = tok, lexeme = lex, number = num, line = lineNumber}

createNonLiteralToken tok lex = createToken tok lex 0 Nothing

charToken x = case x of
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
  r -> error $ "Character `" ++ [r] ++ "` not recognized"
  where
    create tok = createNonLiteralToken tok [x]

stringToDouble x = read x :: Double