{-# LANGUAGE TupleSections #-}

module Discovery where

import Data.List

import qualified Data.HashSet as Set
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import qualified System.IO as FS

import Trie (Trie)
import qualified Trie as T
import ScrabbleBoard

dictPath = "dicts/collins_scrabble_words2019.txt"
wildcardChar = '_'

isInDictionary :: IO (String -> Bool)
isInDictionary = do
  fileContent <- FS.readFile dictPath
  let wordList = L.words fileContent
      dictionary = Set.fromList wordList
  return (`Set.member` dictionary)

searchTrie :: Trie Char [PlaySpot] -> Rack -> WordFragment -> [(String, [PlaySpot])]
searchTrie (T.Node Nothing m)  _ _  | M.null m = []
searchTrie (T.Node (Just v) m) _ acc | M.null m = [(reverse acc, v)]
searchTrie (T.Node Nothing m) rack word = exploreWildMap ++ exploreCharMap
  where
    (wildTries, charTries) = L.partition ((==wildcardChar) . fst) (M.assocs m)
    subracks = nub $ map (\r -> (r, rack\\[r])) rack
    exploreWildMap =  concatMap (\(_, t) -> concatMap (\(c, cs) -> searchTrie t cs (c : word)) subracks) wildTries
    exploreCharMap = concatMap (\(c, t) -> searchTrie t rack (c: word)) charTries

fragsToWords :: Rack -> [(WordFragment, [PlaySpot])] -> [(String, [PlaySpot])]
fragsToWords rack fragmentPlaySpots = searchTrie (T.fromList fragmentPlaySpots) rack []

getCrossWords :: (String, PlaySpot) -> [String]
getCrossWords (word, playspot) = undefined

expandPlaySpots :: [(String, [PlaySpot])] -> [(String, PlaySpot)]
expandPlaySpots = concatMap (\(s, pss) -> map (s,) pss)

-- collect cross words and filter additionally by their validity
validateGroupedPlaySpots :: [(String, [PlaySpot])] -> IO [(String, PlaySpot)]
validateGroupedPlaySpots wordPlaySpots  = do
  isWord <- isInDictionary
  let playSpotsGroupedByValidWord :: [(String, [PlaySpot])]
      playSpotsGroupedByValidWord = filter (isWord . fst) wordPlaySpots
      playSpotsWithValidWord :: [(String, PlaySpot)]
      playSpotsWithValidWord = expandPlaySpots playSpotsGroupedByValidWord
      validPlaySpotWithWord :: [(String, PlaySpot)]
      validPlaySpotWithWord = filter (all isWord . getCrossWords) playSpotsWithValidWord
  return validPlaySpotWithWord
