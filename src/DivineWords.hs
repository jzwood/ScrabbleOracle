module DivineWords where

import Data.HashSet (HashSet)
import Data.List

import qualified Data.HashSet as Set
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import qualified System.IO as FS

import Trie (Trie)
import qualified Trie as T
import ScrabbleBoard

type Rack = String
type Dictionary = HashSet String
type PlaySpot = (String, [WordPlacement])

dictPath = "dicts/collins_scrabble_words2019.txt"
wildcardChar = '_'

dictHashSet :: IO Dictionary
dictHashSet = do
  fileContent <- FS.readFile dictPath
  let wordList = L.words fileContent
  return $ Set.fromList wordList

exploreTrie :: Trie Char [WordPlacement] -> Rack -> String -> [PlaySpot]
exploreTrie (T.Node Nothing m)  _ _  | M.null m = []
exploreTrie (T.Node (Just v) m) _ acc | M.null m = [(reverse acc, v)]
exploreTrie (T.Node Nothing m) rack word = exploreWildMap ++ exploreCharMap
  where
    (wildTries, charTries) = L.partition ((==wildcardChar) . fst) (M.assocs m)
    subracks = nub $ map (\r -> (r, rack\\[r])) rack
    exploreWildMap =  concatMap (\(_, t) -> concatMap (\(c, cs) -> exploreTrie t cs (c : word)) subracks) wildTries
    exploreCharMap = concatMap (\(c, t) -> exploreTrie t rack (c: word)) charTries

findPlayableWords :: Rack -> [PlaySpot] -> IO [PlaySpot]
findPlayableWords rack associationStrings  = do
  let trie = T.fromList associationStrings
      lettersToVal = exploreTrie trie rack []
  dict <- dictHashSet
  return $ filter (\(x, _) -> x `Set.member` dict) lettersToVal
