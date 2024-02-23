module Types
  ( TokenType(..),
    Token(..),
    Literal(..),
    Unary(..),
    Operator(..),
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

data Literal = 
  Number Double |
  String String |
  Boolean Bool |
  Nil

data Unary = 
  MinusUnary | Bang

data Operator = 
  Plus | MinusBinary | Star | Slash | Equal | NotEqual | Greater | GreaterEqual | Less | LessEqual

data Expression = 
  Literal Literal |
  Unary Unary Expression |
  Binary Expression Operator Expression |
  Grouping Expression

instance Show Literal where
  show (Number num) = show num
  show (String str) = show str
  show (Boolean bool) = show bool
  show Nil = "nil"

instance Show Unary where
  show MinusUnary = "-"
  show Bang = "!"

instance Show Operator where
  show Plus = "+"
  show MinusBinary = "-"
  show Star = "*"
  show Slash = "/"
  show Equal = "=="
  show NotEqual = "!="
  show Greater = ">"
  show GreaterEqual = ">="
  show Less = "<"
  show LessEqual = "<="

instance Show Expression where
  show (Literal literal) = show literal
  show (Grouping expression) = parenthesize $ "group " ++ show expression
  show (Unary unaryOp right) = parenthesize $ show unaryOp ++ " " ++ show right
  show (Binary left operator right) = parenthesize $ show operator ++ " " ++ show left ++ " " ++ show right

parenthesize :: String -> String 
parenthesize str = "(" ++ str ++ ")"