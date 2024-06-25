module Parser where

type ParserPosition = Int

newtype Parser token result = Parser { 
    runParser :: [token] -> Maybe (result, [token])
}

instance Functor (Parser token) where
    fmap :: (a -> b) -> Parser token a -> Parser token b
    fmap mapper parser = Parser $ \tokens ->
        case runParser parser tokens of
            Just (result, rest) -> Just (mapper result, rest)
            Nothing -> Nothing

instance Applicative (Parser token) where


instance Monad (Parser token) where


parseChar c = Parser $ \case
    [] -> Nothing
    c':rest -> if c == c' then Just (c', rest) else Nothing

-- parseString = traverse parseChar
