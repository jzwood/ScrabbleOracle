module Scoring where

import Data.List (mapAccumR)
import ScrabbleBoard

-- refactor to simply return score
score :: Board -> (String, Coords) -> (String, Coords, Score)
score board (s, c) = (s, c, 1) -- @TODO

--score' :: Board -> Coords -> Score
--score' board coords =
  --where
    --squares = map unsafeCoordinateToSquare coords

tileToScore :: Tile -> Score
tileToScore (Tile (a, s)) = s

--type Coords = [TileCoordinate]
--newtype Tile = Tile (Char, Integer)
--type Coords = [TileCoordinate]
--newtype Square = Square (Maybe Tile, Maybe Bonus)
--(a -> b -> (a,c)) -> a -> [b] -> (a,[c])
--getCrossPlayspots :: Board -> (String, Coords) -> [(String, [Square])]

scoreTile :: (Score -> Score) -> (Tile, Square) -> (Score -> Score, Score)
scoreTile multiplier (Tile (_, value), Square (Nothing, bonus))
  | bonus == Just TripleLetterScore = (multiplier, value * 3)
  | bonus == Just DoubleLetterScore = (multiplier, value * 2)
  | bonus == Just DoubleWordScore = ((*2) . multiplier, value)
  | bonus == Just TripleWordScore = ((*3) . multiplier, value)
  | otherwise = (multiplier, value)
scoreTile multiplier (_, Square (Just (Tile (_, value)), _)) = (multiplier, value)

scoreWord :: Board -> [Tile] -> [TileCoordinate] -> Score
scoreWord board tiles coordinates = points
  where
    squares = map (unsafeCoordinateToSquare board) coordinates
    (multiplier, valueList) = mapAccumR scoreTile (*1) (zip tiles squares)
    points = multiplier $ sum valueList

--scoreCrossAxis :: (Tile)
--scoreCrossAxes :: Board ->
