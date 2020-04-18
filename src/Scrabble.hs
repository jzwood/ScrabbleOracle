module Scrabble where

import Data.Matrix
import qualified Data.Vector as V
import Data.List (genericLength)
import ScrabbleBoard

type AdjacencyMapping = Matrix Bool

testBoard1 = fromLists
  [
    [Square (Nothing, Nothing), Square (Nothing, Nothing), Square (Nothing, Nothing)],
    [Square (Nothing, Nothing), Square (Just (Tile ('k', 0)), Nothing), Square (Nothing, Nothing)],
    [Square (Nothing, Nothing), Square (Just (Tile ('l', 0)), Nothing), Square (Nothing, Nothing)]
  ]

calcAdjacencies :: Board -> AdjacencyMapping
calcAdjacencies board =
  let
    isAdjacent:: (Int, Int) -> Square -> Bool
    isAdjacent _ (Square (Just (Tile _), _ )) = False
    isAdjacent (r, c) _ = any hasChar $ map (\(x, y) -> safeGet x y board)
      [
        (r, c + 1), (r - 1, c), (r + 1, c), (r, c - 1)
      ]
  in
    mapPos isAdjacent board

exhaustivePlacements :: Board -> [WordPlacement]
exhaustivePlacements board = horizontalWords ++ verticalWords
  where
    rows = nrows board
    cols = ncols board
    wordSizes = [2..7]
    getHWords ws = [[ Coordinate (c + x, r) | x <- [0..(ws - 1)] ] | r <- [0..(rows - 1)], c <- [0..(cols - ws)]]
    getVWords ws = [[ Coordinate (c, r + y) | y <- [0..(ws - 1)] ] | c <- [0..(cols - 1)], r <- [0..(rows - ws)]]
    horizontalWords = concat $ map getHWords wordSizes
    verticalWords = concat $ map getVWords wordSizes


getEveryValidPlacement :: Board -> [WordPlacement]
getEveryValidPlacement board = filter isValidPlacement $ exhaustivePlacements board
  where
    adjacenciesMatrix = calcAdjacencies board
    isValidPlacement :: WordPlacement -> Bool
    isValidPlacement wordPlacements = includesAdjacentSquare && includesEmptySquare
      where
        includesAdjacentSquare = any (\(Coordinate xy) -> adjacenciesMatrix ! xy) wordPlacements
        includesEmptySquare = any (not . hasChar . (coordinateToTile board)) wordPlacements

testBoard2 = fromLists
  [
    [Square (Nothing, Nothing), Square (Nothing, Nothing)],
    [Square (Nothing, Nothing), Square (Just (Tile ('k', 0)), Nothing)]
  ]


main :: IO ()
main = do
  --putStr . show $ calcAdjacencies testBoard1
  putStr . show $ exhaustivePlacements testBoard1
