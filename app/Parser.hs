module Parser where

import Text.Parser
import Control.Applicative
import Control.Monad

data PExpression
    = PTrue
    | PFalse
    | PInteger Int
    | PFloat Double
    | PChar Char
    | PLiteralString String

parseTrue = PTrue <$ str "TRUE"

parseFalse = PFalse <$ str "FALSE"

parseBoolean = parseTrue <|> parseFalse

parseDigit = charThat (\c -> '0' <= c && c <= '9')

parseUnsignedInteger = PInteger . read <$> some parseDigit

-- parseUnsignedFloat = PFloat $ do
--     integerPart <- some parseDigit
--     void $ char '.'
--     decimalPart <- some parseDigit
--     return (read (integerPart ++ "." ++ decimalPart) :: Double)

-- parseUnsignedFloat =
--     parseUnsignedInteger >>= (\integerPart ->
--     void (char '.') >> (
--     parseUnsignedInteger >>= \decimalPart ->
--     return (read (integerPart ++ "." ++ decimalPart) :: Double)))


-- "abc"
-- parseLiteralString = do 
--     _ <- char '\"'
--     s <- many $ charThat (\c -> c /= '\"' && c /= '\'')
--     _ <- char '\"'
--     return $ PLiteralString s

-- parseChar = do
--     _ <- char '\''
--     c <- charThat (\c -> c /= '\"' && c /= '\'')
--     _ <- char '\''
--     return $ PChar c

-- parseDate =

