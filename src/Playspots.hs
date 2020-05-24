module Playspots where

import Data.Matrix
import Data.Maybe
import Data.List (genericLength, groupBy, sortOn)
import ScrabbleBoard

type Adjacencies = Matrix Bool
boardSize = 15
wordSizes = [2..15]


playspotCoords :: Board -> [Coords]
playspotCoords board = horizontalWords ++ verticalWords
  where
    rows = nrows board
    cols = ncols board
    -- isValid: word may not be a subword
    isValid :: Maybe Square -> Maybe Square -> Bool
    isValid sqrBefore sqrAfter = (isNothing sqrBefore || not (hasChar sqrBefore)) && (isNothing sqrAfter || not (hasChar sqrAfter))
    isValidHWord r c ws = isValid (safeGet r (c - 1) board) (safeGet r (c + ws) board)
    isValidVWord r c ws = isValid (safeGet (r - 1) c board) (safeGet (r + ws) c board)

    getHWords ws = [[ Coordinate (r, c + x) | x <- [0..(ws - 1)] ] | r <- [1..rows], c <- [1..(cols - ws + 1)], isValidHWord r c ws]
    getVWords ws = [[ Coordinate (r + y, c) | y <- [0..(ws - 1)] ] | c <- [1..cols], r <- [1..(rows - ws + 1)], isValidVWord r c ws]
    horizontalWords = concatMap getHWords wordSizes
    verticalWords = concatMap getVWords wordSizes

getAdjacencies :: Board -> Adjacencies
getAdjacencies board =
  let
    isAdjacent:: (Int, Int) -> Square -> Bool
    isAdjacent _ (Square (Just (Tile _), _ )) = False
    isAdjacent (r, c) _ = any (hasChar . (\(x, y) -> safeGet x y board))
      [
        (r, c + 1), (r - 1, c), (r + 1, c), (r, c - 1)
      ]
  in
    mapPos isAdjacent board

legalPlayspotCoords :: Board -> [Coords]
legalPlayspotCoords board = filter isValidPlayspot $ playspotCoords board
  where
    adjacencies = getAdjacencies board
    isValidPlayspot :: Coords -> Bool
    isValidPlayspot playspots = includesAdjacentSquare && includesEmptySquare
      where
        includesAdjacentSquare = any (\(Coordinate xy) -> adjacencies ! xy) playspots
        includesEmptySquare = not (all (hasChar . coordinateToSquare board) playspots)

getFragment :: Board -> Coords -> (WordFragment, Coords)
getFragment board playspot = (map (tileToChar . squareToTile mysteryChar . unsafeCoordinateToSquare board) playspot, playspot)

groupCoordsByFragment :: [(WordFragment, Coords)] -> [(WordFragment, [Coords])]
groupCoordsByFragment fragmentWordSpots = map fold' $ group' $ sort' fragmentWordSpots
  where
    sort' = sortOn fst
    group' = groupBy (\(a, _) (b, _) -> a == b)
    fold' = foldr (\(k, v) (_, v') -> (k, v : v')) ("", [])
