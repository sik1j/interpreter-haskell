import Types
import Scanner
import System.Environment
main = do
  args <- getArgs
  if null args
    then putStrLn "Please provide a .lox filename"
    else runFile $ head args

runFile :: String -> IO ()
runFile filename = do
  contents <- readFile filename
  print $ map tokenType $ scanTokens contents

instance Show Token where
  show Token {tokenType = tok, lexeme = lex, literal = lit} = show tok ++ " " ++ lex ++ show lit