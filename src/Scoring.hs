module Scoring where

import Data.List (mapAccumR)
import ScrabbleBoard
import qualified Data.Map as M

-- eventually refactor to simply return score
score :: Board -> (String, Coords) -> (String, Coords, Score)
score board (s, c) = (s, c, score' board s c) -- @TODO

score' :: Board -> String -> Coords -> Score
score' board word = scoreWord board tiles
  where
    tiles = map (\c -> Tile (c, charToValue M.! c)) word

tileToScore :: Tile -> Score
tileToScore (Tile (a, s)) = s

--type Coords = [TileCoordinate]
--newtype Tile = Tile (Char, Integer)
--type Coords = [TileCoordinate]
--newtype Square = Square (Maybe Tile, Maybe Bonus)


scoreTile :: (Score -> Score) -> (Tile, Square) -> (Score -> Score, Score)
scoreTile multiplier (Tile (_, value), Square (Nothing, bonus))
  | bonus == Just TripleLetterScore = (multiplier, value * 3)
  | bonus == Just DoubleLetterScore = (multiplier, value * 2)
  | bonus == Just DoubleWordScore = ((*2) . multiplier, value)
  | bonus == Just TripleWordScore = ((*3) . multiplier, value)
  | otherwise = (multiplier, value)
scoreTile multiplier (_, Square (Just (Tile (_, value)), _)) = (multiplier, value)

scoreWord :: Board -> [Tile] -> Coords -> Score
scoreWord board tiles coordinates = points
  where
    squares = map (unsafeCoordinateToSquare board) coordinates
    (multiplier, valueList) = mapAccumR scoreTile (*1) (zip tiles squares)
    points = multiplier $ sum valueList
