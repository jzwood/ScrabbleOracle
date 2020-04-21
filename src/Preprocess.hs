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


partialW =
  [ "AC__"
  , "A__"
  , "C_T"
  , "C__STS"
  ]
rack = "ACTSO"

--insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
--insert [] v (Node _ map) = Node (Just v) map
--insert (x:xs) v (Node v' map)
  -- | x `M.member` map = Node v' (M.adjust (Trie.insert xs v) x map)
  -- | otherwise = Node v' (M.insert x (Trie.insert xs v Trie.empty) map)

explore :: Trie Char i -> Rack -> [i]
explore (T.Node Nothing m) _ | M.null m = []
explore (T.Node (Just v) m) _ | M.null m = [v]
explore (T.Node Nothing m) rack = exploreWildMap ++ exploreCharMap
  where
    (ktPairsWild, ktPairsChar) = L.partition ((==wildcardChar) . fst) (M.assocs m)
    subracks = nub $ map (\r -> rack\\[r]) rack
    exploreWildMap =  concatMap (\(_, t) -> concatMap (explore t) subracks) ktPairsWild
    exploreCharMap = concatMap (\(k, t) -> explore t rack) ktPairsChar

    --(charmap, wildmap) = M.partitionWithKey (\k _ -> k /= wildcardChar) m
    --vs1 = concatMap (\(k, t) -> explore t rack) (M.assocs charmap)
    --vs2 = if M.null wildmap
          --then concatMap (explore (M.! wildcardChar wildmap)) $ subracks rack
          --else []

--it = initTrie $ zip [0..] partialW
wo = "AC__"
wo2 ="AC__X"

main :: IO ()
main = do
  d <- dictHashSet
  --Prelude.putStr . show $ Set.member w d
  Prelude.putStr . show $ 4
