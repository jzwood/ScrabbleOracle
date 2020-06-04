{-# LANGUAGE OverloadedStrings #-}

module JSONUtils where

import Data.Aeson
import Data.Aeson.Types
import GHC.Exts    -- (fromList)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import Data.Scientific
import qualified Data.Vector as V
import ScrabbleBoard

--newtype Tile = Tile (Char, Integer)
--data Bonus = TripleWordScore | DoubleWordScore | TripleLetterScore | DoubleLetterScore
--newtype Square = Square (Maybe Tile, Maybe Bonus)
--type Board = Matrix Square


-- Vector is the type Aeson uses to represent JSON arrays


{-
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
-}

parseTuple = withObject "tuple" $ \o -> do
  a <- o .: "a"
  b <- o .: "b"
  return (a, b)

parseArray :: Value -> Parser [(String, Bool)]
parseArray (Array arr) = mapM parseTuple (V.toList arr)
parseArray _           = fail "expected an array"


--val :: Value
--val = Object $ fromList [(Number 1, Bool True), (Number 2, Bool True), (Number 3, Bool False)]

wordScoreCoordsToValue :: (String, Score, Coords) -> Value
wordScoreCoordsToValue(word, score, coords) = Object $ fromList
  [
    ("word", String $ T.pack word),
    ("score", Number $ fromInteger score),
    ("coordinates", Array $ fromList $ map (\(Coordinate (x, y)) -> Array $
      fromList [Number $ fromInteger (toInteger x), Number $ fromInteger (toInteger y)]) coords)
  ]

main :: IO()
main = putStr . show $ 3

