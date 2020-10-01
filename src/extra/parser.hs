newtype Parser xs a = Parser { runParser :: xs -> Maybe (a, xs) }

first :: (a -> b) -> (a, c) -> (b, c)
first a2b (a, c) = (a2b a, c)

instance Functor (Parser xs) where
  fmap f Parser {runParser = rp } = Parser { runParser = fmap (first f) . rp }

instance Applicative (Parser xs) where
  pure a = Parser { runParser = \s -> Just (a, s) }
  (<*>) Parser { runParser = rpf } Parser { runParser = rp } = Parser (\s ->
    case rpf s of
        Nothing -> Nothing
        Just (f, rem) ->
          case rp rem of
            Nothing -> Nothing
            Just (a, rem') -> Just (f a, rem'))

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
f <||> g = (||) <$> f <*> g

rowParser :: Parser [String] [Tile]
rowParser = Parser f
  where
    f :: [String] -> Maybe ([Tile], [String])
    f [] = Nothing
    f allRows@(rawRow:tailRows)
      | length allRows /= 15 || length rawRow /= 15 = Nothing
      | isAlpha <||> isUpper <||> (=='_') $ rawRow = Just (map rawCharToSquare rawRow, )
      | otherwise = Nothing

