{-# LANGUAGE TupleSections #-}

module Discovery where

import Data.List
import Data.Tuple
import Data.Maybe

import qualified Data.HashSet as Set
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import qualified System.IO as FS

import Trie (Trie)
import qualified Trie as T
import ScrabbleBoard

dictPath = "dicts/collins_scrabble_words2019.txt"

isInDictionary :: IO (String -> Bool)
isInDictionary = do
  fileContent <- FS.readFile dictPath
  let wordList = L.words fileContent
      dictionary = Set.fromList wordList
  return (`Set.member` dictionary)

searchTrie :: Trie Char [Coords] -> String -> WordFragment -> [(String, [Coords])]
searchTrie (T.Node mc m) rack word = coordsAtNode ++ exploreWildMap ++ exploreCharMap
  where
    coordsAtNode = [(reverse word, fromJust mc) | isJust mc]
    (wildTries, charTries) = L.partition ((==mysteryChar) . fst) (M.assocs m)
    subracks = nub $ map (\r -> (r, rack\\[r])) rack
    exploreWildMap =  concatMap (\(_, t) -> concatMap (\(c, cs) -> searchTrie t cs (c : word)) subracks) wildTries
    exploreCharMap = concatMap (\(c, t) -> searchTrie t rack (c: word)) charTries

fillFrags :: Rack -> [(WordFragment, [Coords])] -> [(String, [Coords])]
fillFrags rack fragmentPlayspots = searchTrie (T.fromList fragmentPlayspots) rack []

getXPlayspot :: Board -> (Int, Int) -> Char -> TileCoordinate -> (String, [Square])
getXPlayspot board (vx, vy) c tile = (xWord, xSquares)
  where
    iter :: (Int, Int) -> [TileCoordinate]
    iter (vx, vy) = iterate (\(Coordinate (x, y)) -> Coordinate (x + vx, y + vy)) tile
    coordinatesToSquares :: [TileCoordinate] -> [Square]
    coordinatesToSquares infTiles = catMaybes $ takeWhile hasChar $ map (coordinateToSquare board) infTiles
    (centerCoord : infCoords) = iter (vx, vy)
    (_ : infCoordsRev) = iter (-vx, -vy)
    centerSquare = unsafeCoordinateToSquare board centerCoord
    squaresBefore = reverse . coordinatesToSquares $ infCoordsRev
    squaresAfter = coordinatesToSquares infCoords
    xSquares = squaresBefore ++ centerSquare : squaresAfter
    xWord = map (tileToChar . squareToTile c) xSquares

-- we filter out crosswords that have already been played b/c
-- A) we don't want to score it and
-- B) someone could have played an unchallenged illegal word
getXPlayspots :: Board -> (String, Coords) -> [(String, [Square])]
getXPlayspots board (word, playspot) = actualXSpots
  where
    xVector = swap $ toVector (head playspot) (playspot !! 1)  -- all words are at least 2 chars long
    charCoords = filter (not . unsafeHasChar . unsafeCoordinateToSquare board . snd) (zip word playspot)
    xSpots = map (uncurry (getXPlayspot board xVector)) charCoords
    actualXSpots = filter ((>1) . length . fst) xSpots

expandPlayspots :: [(String, [Coords])] -> [(String, Coords)]
expandPlayspots = concatMap (\(s, pss) -> map (s,) pss)

-- collect cross words and filter additionally by their validity
validateGroupedPlayspots :: Board -> [(String, [Coords])] -> IO [(String, Coords)]
validateGroupedPlayspots board wordPlayspots  = do
  isWord <- isInDictionary
  let playspotsGroupedByValidWord :: [(String, [Coords])]
      playspotsGroupedByValidWord = filter (isWord . fst) wordPlayspots
      playspotsWithValidWord :: [(String, Coords)]
      playspotsWithValidWord = expandPlayspots playspotsGroupedByValidWord
      validPlayspotWithWord :: [(String, Coords)]
      validPlayspotWithWord = filter (all (isWord . fst) . getXPlayspots board) playspotsWithValidWord
  return validPlayspotWithWord
