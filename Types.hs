module Types
  ( TokenType(..),
    Literal(..),
    Token(..)
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

data Literal = StringLiteral String | NumberLiteral Double deriving(Show)

data Token = Token
  { tokenType :: TokenType,
    lexeme :: String,
    literal :: Maybe Literal,
    line :: Int
  }

instance Show Token where
  show Token {tokenType = tok, lexeme = lex, literal = lit} = show tok ++ " " ++ lex ++ show lit