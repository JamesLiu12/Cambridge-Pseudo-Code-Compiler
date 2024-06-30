module Parser where

import Text.Parser
import Control.Applicative
import Control.Monad
import Data.Char

data PExpression
    = PTrue
    | PFalse
    | PInteger Int
    | PFloat Double
    | PLiteralChar Char
    | PLiteralString String

parseTrue :: Parser error PExpression
parseTrue = PTrue <$ str "TRUE"

parseFalse :: Parser error PExpression
parseFalse = PFalse <$ str "FALSE"

parseBoolean :: Parser error PExpression
parseBoolean = parseTrue <|> parseFalse

parseDigit :: Parser error Char
parseDigit = charThat isDigit

parseUnsignedInteger :: Parser error PExpression
parseUnsignedInteger = PInteger . read <$> some parseDigit

parseUnsignedFloat :: Parser error PExpression
parseUnsignedFloat = PFloat <$> do
    integerPart <- some parseDigit
    void $ char '.'
    decimalPart <- some parseDigit
    return (read (integerPart ++ "." ++ decimalPart) :: Double)

parseLiteralString :: Parser error PExpression
parseLiteralString = PLiteralString <$> do
    void $ char '\"'
    s <- many $ charThat (\c -> c /= '\"' && c /= '\'')
    void $ char '\"'
    return s

parseLiteralChar :: Parser error PExpression
parseLiteralChar = PLiteralChar <$> do
    void $ char '\''
    c <- charThat (\c -> c /= '\"' && c /= '\'')
    void $ char '\''
    return c

-- parseDate =

