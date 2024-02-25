import Types
import Scanner
import Parser
import System.Environment
import Control.Monad
main = do
  args <- getArgs
  if null args
    then runPrompt
    else runFile $ head args

runPrompt :: IO ()
runPrompt = forever $ do
  putStr "> "
  line <- getLine
  let tokens = scanTokens line
  print $ map tokenType tokens
  print $ parseTokens tokens

runFile :: String -> IO ()
runFile filename = do
  contents <- readFile filename
  let tokens = scanTokens contents
  print $ map tokenType tokens
  print $ parseTokens tokens