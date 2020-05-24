module Scoring where

import Data.List (mapAccumR)
import ScrabbleBoard
import Discovery
import qualified Data.Map as M

-- eventually refactor to simply return score
score :: Board -> (String, Coords) -> (String, Coords, Score)
score board (s, c) = (s, c, score' board s c) -- @TODO

score' :: Board -> String -> Coords -> Score
score' board word coords = mainAxispoints + xAxisPoints
  where
    squares = map (unsafeCoordinateToSquare board) coords
    mainAxispoints = scoreWord board word squares
    xPlayspots = getXPlayspots board (word, coords)
    xAxisPoints = sum $ map (uncurry (scoreWord board)) xPlayspots

wordToVals :: String -> [Integer]
wordToVals = map (charToValue M.!)

scoreTile :: (Score -> Score) -> (Integer, Square) -> (Score -> Score, Score)
scoreTile multiplier (value, Square (Nothing, bonus))
  | bonus == Just TripleLetterScore = (multiplier, value * 3)
  | bonus == Just DoubleLetterScore = (multiplier, value * 2)
  | bonus == Just DoubleWordScore = ((*2) . multiplier, value)
  | bonus == Just TripleWordScore = ((*3) . multiplier, value)
  | otherwise = (multiplier, value)
scoreTile multiplier (_, Square (Just (Tile (_, value)), _)) = (multiplier, value)


scoreWord :: Board -> String -> [Square] -> Score
scoreWord board word squares = points
  where
    values = wordToVals word
    (multiplier, valueList) = mapAccumR scoreTile (*1) (zip values squares)
    points = multiplier $ sum valueList
