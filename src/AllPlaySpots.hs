module AllPlaySpots where

import Data.Matrix
import Data.Maybe
import Data.List (genericLength, groupBy, sortOn)
import ScrabbleBoard

type Adjacencies = Matrix Bool
boardSize = 15
wordSizes = [2..15]


allPlaySpots :: Board -> [PlaySpot]
allPlaySpots board = horizontalWords ++ verticalWords
  where
    rows = nrows board
    cols = ncols board
    -- isValid: word may not be a subword
    isValid :: Maybe Square -> Maybe Square -> Bool
    isValid sqrBefore sqrAfter = (isNothing sqrBefore || not (hasChar sqrBefore)) && (isNothing sqrAfter || not (hasChar sqrAfter))
    isValidHWord c r ws = isValid (safeGet (c - 1) r board) (safeGet (c + ws) r board)
    isValidVWord c r ws = isValid (safeGet c (r - 1) board) (safeGet c (r + ws) board)

    getHWords ws = [[ Coordinate (c + x, r) | x <- [0..(ws - 1)] ] | r <- [0..(rows - 1)], c <- [0..(cols - ws)], isValidHWord c r ws]
    getVWords ws = [[ Coordinate (c, r + y) | y <- [0..(ws - 1)] ] | c <- [0..(cols - 1)], r <- [0..(rows - ws)], isValidVWord c r ws]
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

findPlaySpots :: Board -> [PlaySpot]
findPlaySpots board = filter isValidPlaySpot $ allPlaySpots board
  where
    adjacencies = getAdjacencies board
    isValidPlaySpot :: PlaySpot -> Bool
    isValidPlaySpot playSpots = includesAdjacentSquare && includesEmptySquare
      where
        includesAdjacentSquare = any (\(Coordinate xy) -> adjacencies ! xy) playSpots
        includesEmptySquare = not (all (hasChar . coordinateToSquare board) playSpots)

getFragment :: Board -> PlaySpot -> (WordFragment, PlaySpot)
getFragment board playSpot = (map (tileToChar . squareToTile wildcardChar . unsafeCoordinateToSquare board) playSpot, playSpot)

groupWordSpotsByFragment :: [(WordFragment, PlaySpot)] -> [(WordFragment, [PlaySpot])]
groupWordSpotsByFragment fragmentWordSpots = map fold' $ group' $ sort' fragmentWordSpots
  where
    sort' = sortOn fst
    group' = groupBy (\(a, _) (b, _) -> a == b)
    fold' = foldr (\(k, v) (_, v') -> (k, v : v')) ("", [])
