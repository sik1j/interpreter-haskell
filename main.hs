import Control.Monad (forM_)
import Data.Char
import Data.List
import System.Environment
import Types (Literal (..), Token (..), TokenType (..))

main = do
  args <- getArgs
  if null args
    then putStrLn "Please provide a .lox filename"
    else runFile $ head args

runFile :: String -> IO ()
runFile filename = do
  contents <- readFile filename
  print $ map lexeme $ scanTokens contents

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

    addString code = createToken STRING str 0 (Just $ StringLiteral str) : scanTokens rest
      where
        (str, _ : rest) = span (/= '"') code

    addNumber code = createToken STRING strNum 0 (Just $ NumberLiteral num) : scanTokens rest
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
    addIdentifier code = createToken IDENTIFIER iden 0 Nothing : scanTokens rest
      where (iden, rest) = span (\x -> isAlphaOrUnderScore x || isDigit x) code

createToken tok lex lineNumber lit = Token {tokenType = tok, lexeme = lex, literal = lit, line = lineNumber}

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

instance Show Token where
  show Token {tokenType = tok, lexeme = lex, literal = lit} = show tok ++ " " ++ lex ++ show lit