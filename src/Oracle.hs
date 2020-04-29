module Oracle where

import Data.List (sortOn)

import Discovery
import AllPlaySpots
import Scoring
import ScrabbleBoard

oracle :: Board -> Rack -> IO [(PlaySpot, Score)]
oracle board rack = do
  let playSpots :: [PlaySpot]
      playSpots = findPlaySpots board
      playSpotsWithFragments :: [(WordFragment, PlaySpot)]
      playSpotsWithFragments = map (getFragment board) playSpots
      wordSpotsGroupedByFragment :: [(WordFragment, [PlaySpot])]
      wordSpotsGroupedByFragment = groupWordSpotsByFragment playSpotsWithFragments
      playSpotsGroupedByWord :: [(WordFragment, [PlaySpot])]
      playSpotsGroupedByWord = fragsToWords rack wordSpotsGroupedByFragment
  -- validPlaySpotsWithWords :: IO [(String, PlaySpot)]
  validPlaySpotsWithWords <- validateGroupedPlaySpots playSpotsGroupedByWord
  return $ sortOn (negate . snd) $ map (score board) validPlaySpotsWithWords

main :: IO ()
main = do
  res <- oracle [] "CAT"
  Prelude.putStr . show $ res
