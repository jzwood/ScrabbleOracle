module Scoring where

import Data.List (mapAccumR)
import ScrabbleBoard

score :: Board -> (String, Coords) -> (Coords, Score)
score = undefined

tileToScore :: Tile -> Score
tileToScore (Tile (a, s)) = s

scoreWord :: [Square] -> Score
scoreWord = undefined

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

scoreMainAxis :: Board -> [Tile] -> [TileCoordinate] -> Score
scoreMainAxis board tiles coordinates = points
  where
    squares = map (unsafeCoordinateToSquare board) coordinates
    (multiplier, valueList) = mapAccumR scoreTile (*1) (zip tiles squares)
    points = multiplier $ sum valueList

--scoreCrossAxis :: (Tile)
--scoreCrossAxes :: Board ->
