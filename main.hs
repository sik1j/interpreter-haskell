import System.Environment
import Data.List
import Control.Monad (forM_)
import Types

main = do
  args <- getArgs
  if null args
    then putStrLn "Please provide a .lox filename"
    else runFile $ head args

runFile :: String -> IO ()
runFile filename = do
  contents <- readFile filename
  forM_ (words contents) (putStr . ( ++ " "))

instance Show Token where
  show Token{tokenType=tok, lexeme=lex, literal=lit } = show tok ++ " " ++ lex ++ " " ++ show lit