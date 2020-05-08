module Oracle where

import Data.List (sortOn)

import Discovery
import AllPlaySpots
import Scoring
import ScrabbleBoard

oracle :: Board -> RackLetters -> IO [(Coords, Score)]
oracle board rack = do
  let playSpots :: [Coords]
      playSpots = findPlaySpots board
      playSpotsWithFragments :: [(WordFragment, Coords)]
      playSpotsWithFragments = map (getFragment board) playSpots
      wordSpotsGroupedByFragment :: [(WordFragment, [Coords])]
      wordSpotsGroupedByFragment = groupWordSpotsByFragment playSpotsWithFragments
      playSpotsGroupedByWord :: [(WordFragment, [Coords])]
      playSpotsGroupedByWord = fragsToWords rack wordSpotsGroupedByFragment
  -- validPlaySpotsWithWords :: IO [(String, Coords)]
  validPlaySpotsWithWords <- validateGroupedPlaySpots board playSpotsGroupedByWord
  return $ sortOn (negate . snd) $ map (score board) validPlaySpotsWithWords

main :: IO ()
main = do
  res <- oracle [] "CAT"
  Prelude.putStr . show $ res
