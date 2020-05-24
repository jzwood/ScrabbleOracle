module Oracle where

import Data.List (sortOn)

import Discovery
import Playspots
import Scoring
import ScrabbleBoard


getCoordinatesGroupedByStr :: Board -> Rack -> [(String, [Coords])]
getCoordinatesGroupedByStr board rack = coordsGroupedByStr
  where
    playspotCoords = legalPlayspotCoords board
    coordsGroupedByFragment = groupCoordsByFragment $ map (getFragment board) playspotCoords
    coordsGroupedByStr = fillFrags rack coordsGroupedByFragment

oracle :: Board -> Rack -> IO [(String, Score, Coords)]
oracle board rack = do
  let coordsGroupedByStr = getCoordinatesGroupedByStr board rack
  -- validPlayspotsWithWords :: IO [(String, Coords)]
  validPlayspotsWithWords <- validateGroupedPlayspots board coordsGroupedByStr
  let scores = map (uncurry (score board)) validPlayspotsWithWords
      (words, coords) = unzip validPlayspotsWithWords
      rankedPlayspots = sortOn (negate . (\(_, a, _) -> a)) $ zip3 words scores coords
  return rankedPlayspots
