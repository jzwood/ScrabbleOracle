module Placements where

import Data.Matrix
import Data.Maybe
import Data.List (genericLength, groupBy, sortOn)
import ScrabbleBoard

type AdjacencyMapping = Matrix Bool
wordSizes = [2..9]

exhaustivePlacements :: Board -> [WordPlacement]
exhaustivePlacements board = horizontalWords ++ verticalWords
  where
    rows = nrows board
    cols = ncols board
    getHWords ws = [[ Coordinate (c + x, r) | x <- [0..(ws - 1)] ] | r <- [0..(rows - 1)], c <- [0..(cols - ws)]]
    getVWords ws = [[ Coordinate (c, r + y) | y <- [0..(ws - 1)] ] | c <- [0..(cols - 1)], r <- [0..(rows - ws)]]
    horizontalWords = concatMap getHWords wordSizes
    verticalWords = concatMap getVWords wordSizes

calcAdjacencies :: Board -> AdjacencyMapping
calcAdjacencies board =
  let
    isAdjacent:: (Int, Int) -> Square -> Bool
    isAdjacent _ (Square (Just (Tile _), _ )) = False
    isAdjacent (r, c) _ = any (hasChar . (\(x, y) -> safeGet x y board))
      [
        (r, c + 1), (r - 1, c), (r + 1, c), (r, c - 1)
      ]
  in
    mapPos isAdjacent board

findPlacements :: Board -> [WordPlacement]
findPlacements board = filter isValidPlacement $ exhaustivePlacements board
  where
    adjacenciesMatrix = calcAdjacencies board
    isValidPlacement :: WordPlacement -> Bool
    isValidPlacement wordPlacements = includesAdjacentSquare && includesEmptySquare
      where
        includesAdjacentSquare = any (\(Coordinate xy) -> adjacenciesMatrix ! xy) wordPlacements
        includesEmptySquare = not (all (hasChar . coordinateToSquare board) wordPlacements)

toPlaySpotCandidate :: Board -> WordPlacement -> (String, WordPlacement)
toPlaySpotCandidate board placement = (map (tileToChar . squareToTile . unsafeCoordinateToSquare board) placement, placement)

groupCandidates :: [(String, WordPlacement)] -> [(String, [WordPlacement])]
groupCandidates candidates = map fold' $ group' $ sort' candidates
  where
    sort' = sortOn fst
    group' = groupBy (\(a, _) (b, _) -> a == b)
    fold' = foldr (\(k, v) (_, v') -> (k, v : v')) ('_', [])
