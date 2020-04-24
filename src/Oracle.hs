module Oracle where

import Placements
import Scoring
import ScrabbleBoard
import PlaySpots

oracle :: Board -> Rack -> IO [(WordPlacement, Score)]
oracle board rack = do
  let placements :: [WordPlacement]
      placements = findPlacements board
      spots :: [PlaySpot]
      spots = map placementToSpot placements
  playSpots <- findPlaySpots rack spots
  let score = placementToScore playSpots


main :: IO ()
main = do
  res <- findPlayableWords rack associationStrings
  Prelude.putStr . show $ res
