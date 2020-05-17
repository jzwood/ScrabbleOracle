module Oracle where

import Data.List (sortOn)

import Discovery
import Playspots
import Scoring
import ScrabbleBoard

{--
  1. give me the coordinates of every legal place to play (i.e. physically legal, no dictionary checks yet)
  2. pair each of these with their corresponding board string fragment (fragment example: "_t__")
  3. group these pairings by word fragment -- now each word fragment will be associated with 1+ playspot coordinates
  4. for each fragment and playspot coords find every possible string combo that the input rack can produce
  6. remove from list of letter combo / playspot coords any pairing that, if played, produces a word not in the dictionary
  7. score all remaining valid playable spots and order from highest point value to lowest
--}

oracle :: Board -> Rack -> IO [(Coords, Score)]
oracle board rack = do
  let playspotCoords :: [Coords]
      playspotCoords = legalPlayspotCoords board
      coordsGroupedByFragment :: [(WordFragment, [Coords])]
      coordsGroupedByFragment = groupCoordsByFragment $ map (getFragment board) playspotCoords
      coordsGroupedByStr :: [(String, [Coords])]
      coordsGroupedByStr = fillFrags (toLetters rack) coordsGroupedByFragment
  -- validPlayspotsWithWords :: IO [(String, Coords)]
  validPlayspotsWithWords <- validateGroupedPlayspots board coordsGroupedByStr
  return $ sortOn (negate . snd) $ map (score board) validPlayspotsWithWords

main :: IO ()
main = do
  res <- oracle [] "CAT"
  Prelude.putStr . show $ res
