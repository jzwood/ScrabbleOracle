module Oracle where

import Placements
import Scoring
import ScrabbleBoard
import PlaySpots

oracle :: Board -> Rack -> IO [(WordPlacement, Score)]
oracle board rack = do
  let placements :: [WordPlacement]
      placements = findPlacements board
      playSpotCandidates :: [(String, WordPlacement)]
      playSpotCandidates = map toPlaySpotCandidate placements
      groupedCandidates :: [(String, [WordPlacement])]
      groupedCandidates = groupCandidates playSpotCandidates
  playSpots <- findPlaySpots rack groupedCandidates
  let scoredPlacements :: [(WordPlacement, Score)]
      scoredPlacements = concatMap (\(word, placements) -> map ((,) word . placementToScore board word) placements) playSpots
  return scoredPlacements


main :: IO ()
main = do
  res <- findPlayableWords rack associationStrings
  Prelude.putStr . show $ res
