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

searchTrie :: Trie Char [Coords] -> RackLetters -> WordFragment -> [(String, [Coords])]
searchTrie (T.Node Nothing m)  _ _  | M.null m = []
searchTrie (T.Node (Just v) m) _ acc | M.null m = [(reverse acc, v)]
searchTrie (T.Node Nothing m) rack word = exploreWildMap ++ exploreCharMap
  where
    (wildTries, charTries) = L.partition ((==wildcardChar) . fst) (M.assocs m)
    subracks = nub $ map (\r -> (r, rack\\[r])) rack
    exploreWildMap =  concatMap (\(_, t) -> concatMap (\(c, cs) -> searchTrie t cs (c : word)) subracks) wildTries
    exploreCharMap = concatMap (\(c, t) -> searchTrie t rack (c: word)) charTries

fragsToWords :: RackLetters -> [(WordFragment, [Coords])] -> [(String, [Coords])]
fragsToWords rack fragmentPlaySpots = searchTrie (T.fromList fragmentPlaySpots) rack []

getCrossPlaySpot :: Board -> (Int, Int) -> Char -> TileCoordinate -> (String, [Square])
getCrossPlaySpot board (vx, vy) c tile = (crossWord, crossSquares)
  where
    iter :: (Int, Int) -> [TileCoordinate]
    iter (vx, vy) = iterate (\(Coordinate (x, y)) -> Coordinate (x + vx, y + vy)) tile
    coordinatesToSquares :: [TileCoordinate] -> [Square]
    coordinatesToSquares infTiles = catMaybes $ takeWhile isJust $ map (coordinateToSquare board) infTiles
    backwards = reverse $ coordinatesToSquares $ iter (vx, vy)
    forwards = coordinatesToSquares $ iter (-vx, -vy)
    crossSquares = reverse backwards ++ tail forwards
    crossWord = map (tileToChar . squareToTile c) crossSquares

getCrossPlaySpots :: Board -> (String, Coords) -> [(String, [Square])]
getCrossPlaySpots board (word, playSpot) = zipWith (getCrossPlaySpot board xVector) word playSpot
  where
    xVector = swap $ toVector (head playSpot) (playSpot !! 1)  -- all words are at least 2 chars long

expandPlaySpots :: [(String, [Coords])] -> [(String, Coords)]
expandPlaySpots = concatMap (\(s, pss) -> map (s,) pss)

-- collect cross words and filter additionally by their validity
validateGroupedPlaySpots :: Board -> [(String, [Coords])] -> IO [(String, Coords)]
validateGroupedPlaySpots board wordPlaySpots  = do
  isWord <- isInDictionary
  let playSpotsGroupedByValidWord :: [(String, [Coords])]
      playSpotsGroupedByValidWord = filter (isWord . fst) wordPlaySpots
      playSpotsWithValidWord :: [(String, Coords)]
      playSpotsWithValidWord = expandPlaySpots playSpotsGroupedByValidWord
      validPlaySpotWithWord :: [(String, Coords)]
      validPlaySpotWithWord = filter (all (isWord . fst) . getCrossPlaySpots board) playSpotsWithValidWord
  return validPlaySpotWithWord
