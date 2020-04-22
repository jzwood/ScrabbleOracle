module Preprocess where

import Data.HashSet (HashSet)
import Data.List

import qualified Data.HashSet as Set
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import qualified System.IO as FS
import Trie (Trie)
import qualified Trie as T

type Rack = String
type Dictionary = HashSet String

dictPath = "dicts/collins_scrabble_words2019.txt"
wildcardChar = '_'



dictHashSet :: IO (HashSet String)
dictHashSet = do
  fileContent <- FS.readFile dictPath
  let wordList = L.words fileContent
  return $ Set.fromList wordList

--initTrie :: [(String, String)] -> Trie String Integer
--initTrie kstrs = T.fromList kstrs



explore :: Trie Char i -> Rack -> [i]
explore (T.Node Nothing m) _ | M.null m = []
explore (T.Node (Just v) m) _ | M.null m = [v]
explore (T.Node Nothing m) rack = exploreWildMap ++ exploreCharMap
  where
    (ktPairsWild, ktPairsChar) = L.partition ((==wildcardChar) . fst) (M.assocs m)
    subracks = nub $ map (\r -> rack\\[r]) rack
    exploreWildMap =  concatMap (\(_, t) -> concatMap (explore t) subracks) ktPairsWild
    exploreCharMap = concatMap (\(k, t) -> explore t rack) ktPairsChar

explore' :: Trie Char v -> Rack -> String -> [(String, v)]
explore' (T.Node Nothing m)  _ _  | M.null m = []
explore' (T.Node (Just v) m) _ acc | M.null m = [(acc, v)]
explore' (T.Node Nothing m) rack word = exploreWildMap ++ exploreCharMap
  where
    (wildTries, charTries) = L.partition ((==wildcardChar) . fst) (M.assocs m)
    subracks = nub $ map (\r -> (r, rack\\[r])) rack
    exploreWildMap =  concatMap (\(_, t) -> concatMap (\(c, cs) -> explore' t cs (c : word)) subracks) wildTries
    exploreCharMap = concatMap (\(c, t) -> explore' t rack (c: word)) charTries

partialW =
  [ "AC__"
  , "A__"
  , "C_T"
  , "C__STS"
  ]
rack = "ACTSO"
it = T.fromList $ zip partialW [0..]
e = explore' it rack []
e' :: [String]
e' = map (\(word, i) -> reverse word) e

final :: [String] -> IO [String]
final words = do
  dict <- dictHashSet
  return $ filter (`Set.member` dict) words

main :: IO ()
main = do
  res <- final e'
  Prelude.putStr . show $ res
