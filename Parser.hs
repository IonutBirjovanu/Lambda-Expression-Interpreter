module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Data.Char
import Expr

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    mp >>= f = Parser $
        \s -> case parse mp s of
            Nothing -> Nothing
            Just (val, rest) -> parse (f val) rest
    return x = Parser $ \s -> Just (x, s)


instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
                               Nothing -> parse p2 s
                               x -> x

--- type declaration over ---

failParser :: Parser a
failParser = Parser $ \s -> Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $
  \s -> case s of 
          [] -> Nothing
          (x:xs) -> if p x then Just (x,xs) else Nothing
          
charParser :: Char -> Parser Char
charParser c = Parser $ \s -> case s of 
                                [] -> Nothing
                                (x:xs) -> if x == c then Just (c,xs) else Nothing

starParser :: (Parser a) -> Parser [a]
starParser p = (plusParser p) <|> (return [])

plusParser :: (Parser a) -> Parser [a]
plusParser p =
  do x <- p
     xs <- starParser p
     return (x:xs)

whiteSpaceParser :: Parser String
whiteSpaceParser = starParser (charParser ' ')

constructApplication :: [Expr] -> Expr
constructApplication = foldl1 Application

variableParser :: Parser Expr
variableParser = do
    x <- plusParser (predicateParser isAlpha)
    whiteSpaceParser
    return $ Variable x

applicationParser :: Parser Expr
applicationParser = do
    charParser '('
    whiteSpaceParser
    lista_expr <- plusParser exprParser
    charParser ')'
    whiteSpaceParser
    return $ constructApplication lista_expr

functionParser :: Parser Expr
functionParser = do
    whiteSpaceParser
    charParser '\\'
    v <- variableParser
    charParser '.'
    e <- exprParser
    return $ Function (getVariable v) e
    where
        getVariable (Variable v) = v

macroParser :: Parser Expr
macroParser = do
    charParser '$'
    name <- plusParser (predicateParser isAlpha)
    whiteSpaceParser
    return $ Macro name

exprParser :: Parser Expr
exprParser = applicationParser <|> functionParser <|> variableParser <|> macroParser

-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr s = case parse (plusParser exprParser) s of
    Just (resultList, _) -> constructApplication resultList
    Nothing -> error "Failed to parse the expression."

assignParser :: Parser Code
assignParser = do
    name <- plusParser (predicateParser isLower)
    whiteSpaceParser
    charParser '='
    whiteSpaceParser
    listaExpr <- plusParser exprParser
    return $ Assign name (constructApplication listaExpr)

evaluateParser :: Parser Code
evaluateParser = do
    listaExpr <- plusParser exprParser
    return $ Evaluate (constructApplication listaExpr)

codeParser :: Parser Code
codeParser = assignParser <|> evaluateParser

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code s = case parse codeParser s of
    Just (result, _) -> result
    Nothing -> error "Failed to parse the code line."
