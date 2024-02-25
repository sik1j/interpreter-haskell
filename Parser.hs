module Parser (parseTokens) where

import Data.Maybe
import Types (Comparison (..), Equality (..), Expression (..), Factor (..), Primary (..), Term (..), Token (..), TokenType (..), Unary (..))

type Operator = Token

parseTokens :: [Token] -> [Expression]
parseTokens [Token {tokenType = EOF}] = []
parseTokens l = expr : parseTokens rest
  where
    (expr, rest) = expression l

expression :: [Token] -> (Expression, [Token])
expression = equality


equality :: [Token] -> (Expression, [Token])
equality [x, eof] = comparison [x, eof]
equality l = let (leftExpr, rest) = comparison l 
    in case rest of
        (x:xs) | x `isOperator` [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] -> (computeBinary leftExpr x rightExpr, rest1)
            where (rightExpr, rest1) = equality xs
        _ -> (leftExpr, rest)

comparison :: [Token] -> (Expression, [Token])
comparison [x, eof] = term [x, eof]
comparison l = let (leftExpr, rest) = term l 
    in case rest of
        (x:xs) | x `isOperator` [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] -> (computeBinary leftExpr x rightExpr, rest1)
            where (rightExpr, rest1) = comparison xs
        _ -> (leftExpr, rest)

term :: [Token] -> (Expression, [Token])
term [x, eof] = factor [x, eof]
term l = let (leftExpr, rest) = factor l 
    in case rest of
        (x:xs) | x `isOperator` [PLUS, MINUS] -> (computeBinary leftExpr x rightExpr, rest1)
            where (rightExpr, rest1) = term xs
        _ -> (leftExpr, rest)

factor :: [Token] -> (Expression, [Token])
factor [x, eof] = unary [x, eof]
factor l = let (leftExpr, rest) = unary l 
    in case rest of
        (x:xs) | x `isOperator` [STAR, SLASH] -> (computeBinary leftExpr x rightExpr, rest1)
            where (rightExpr, rest1) = factor xs
        _ -> (leftExpr, rest)

unary :: [Token] -> (Expression, [Token])
unary [x, eof] = primary [x, eof]
unary l@(x : xs)
    | x `isOperator` [MINUS, BANG] = (computeUnary x rightExpr, rest)
    | otherwise = primary l
    where
        (rightExpr, rest) = unary xs
unary _ = error "Unary: Invalid input"

primary :: [Token] -> (Expression, [Token])
primary ((Token {tokenType = tok, lexeme = lex, number = num}) : xs) = case tok of
  NUMBER -> createExpr (Number $ fromJust num)
  STRING -> createExpr (String lex)
  TRUE -> createExpr (Bool True)
  FALSE -> createExpr (Bool False)
  NIL -> createExpr Nil
  LEFT_PAREN ->
    let (expr, rest) = expression xs
     in if tokenType (head rest) == RIGHT_PAREN
          then (PrimaryExpr $ Grouping expr, tail rest)
          else error "Primary: Unmatched parenthesis"
  where
    createExpr primaryExpr = (PrimaryExpr primaryExpr, xs)

computeUnary :: Operator ->  Expression -> Expression
computeUnary t@Token{tokenType = tok} (PrimaryExpr expr) = case (tok, expr) of
    (MINUS, Number n) -> PrimaryExpr $ Number (-n)
    (BANG, Bool b) -> PrimaryExpr $ Bool (not b)
    (_, Grouping e) -> computeUnary t e
    (_,_) -> error "Invalid unary operation"

computeBinary :: Expression -> Operator -> Expression -> Expression
computeBinary (PrimaryExpr (Grouping left)) op right = computeBinary left op right
computeBinary left op (PrimaryExpr (Grouping right)) = computeBinary left op right
computeBinary (PrimaryExpr (Number left)) op (PrimaryExpr (Number right)) = case tokenType op of
    PLUS -> PrimaryExpr $ Number (left + right)
    MINUS -> PrimaryExpr $ Number (left - right)
    STAR -> PrimaryExpr $ Number (left * right)
    SLASH -> PrimaryExpr $ Number (left / right)
    _ -> error "Invalid binary operation"

isOperator :: Token -> [TokenType] -> Bool
isOperator Token{tokenType = tok} = (tok `elem`)