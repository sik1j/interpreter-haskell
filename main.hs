import Types
import Scanner
import System.Environment
main = do
  -- let expr = Binary (Unary MinusUnary (Literal $ Number 123)) Star (Grouping (Literal $ Number 45.67))
  -- let expr = Literal (NUMBER_LIT 1.0)
  -- print expr
  args <- getArgs
  if null args
    then putStrLn "Please provide a .lox filename"
    else runFile $ head args

runFile :: String -> IO ()
runFile filename = do
  contents <- readFile filename
  print $ map (\x -> (lexeme x, line x)) $ scanTokens 1 contents