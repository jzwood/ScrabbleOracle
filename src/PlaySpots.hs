module PlaySpots where

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

exploreTrie :: Trie Char [WordPlacement] -> Rack -> String -> [PlaySpot]
exploreTrie (T.Node Nothing m)  _ _  | M.null m = []
exploreTrie (T.Node (Just v) m) _ acc | M.null m = [(reverse acc, v)]
exploreTrie (T.Node Nothing m) rack word = exploreWildMap ++ exploreCharMap
  where
    (wildTries, charTries) = L.partition ((==wildcardChar) . fst) (M.assocs m)
    subracks = nub $ map (\r -> (r, rack\\[r])) rack
    exploreWildMap =  concatMap (\(_, t) -> concatMap (\(c, cs) -> exploreTrie t cs (c : word)) subracks) wildTries
    exploreCharMap = concatMap (\(c, t) -> exploreTrie t rack (c: word)) charTries

findPlaySpots :: Rack -> [PlaySpot] -> IO [PlaySpot]
findPlaySpots rack associationStrings  = do
  let trie = T.fromList associationStrings
      playSpots = exploreTrie trie rack []
  isWord <- isInDictionary
  return $ filter (isWord . fst) playSpots
