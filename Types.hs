module Types
  ( TokenType(..),
    Token(..),
    Unary(..),
    Expression(..)
  ) 
where

data TokenType = 
  -- Single-chracter toekns.
  LEFT_PAREN| RIGHT_PAREN| LEFT_BRACE| RIGHT_BRACE|
  COMMA| DOT| MINUS| PLUS| SEMICOLON| SLASH| STAR

  -- One or two character tokens.
  |BANG| BANG_EQUAL|
  EQUAL| EQUAL_EQUAL|
  GREATER| GREATER_EQUAL|
  LESS| LESS_EQUAL

  -- Literals.
  |IDENTIFIER| STRING| NUMBER

  -- Keywords.
  |AND| CLASS| ELSE| FALSE| FUN| FOR| IF| NIL| OR|
  PRINT| RETURN| SUPER| THIS| TRUE| VAR| WHILE

  |EOF  deriving(Eq, Show)

data Token = Token
  { tokenType :: TokenType,
    lexeme :: String,
    number :: Maybe Double,
    line :: Int
  }

instance Show Token where
  show Token {tokenType = tok, lexeme = lex, number = lit} = show tok ++ " " ++ lex ++ show lit

data Expression = 
  Equality Equality

data Equality = 
  Comparison Comparison | EqualEqual Comparison Comparison | NotEqual Comparison Comparison

data Comparison =
  Term Term | Greater Term Term | GreaterEqual Term Term | Less Term Term | LessEqual Term Term

data Term = 
  Factor Factor | Plus Factor Factor | MinusBinary Factor Factor

data Factor = 
  Unary Unary | Divide Unary Unary | Multiply Unary Unary

data Unary = 
  MinusUnary Unary | Bang Unary | 
  Primary Primary

data Primary = 
  Number Double |
  String String |
  Boolean Bool |
  Nil |
  Grouping Expression

parenthesize :: String -> String 
parenthesize str = "(" ++ str ++ ")"