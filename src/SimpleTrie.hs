module SimpleTrie where

import Data.Map (Map)
import qualified Data.Map.Lazy as M

data Trie k = Node Bool (Map k (Trie k))
  deriving (Show, Eq)

empty :: (Ord k) => Trie k
empty = Node False M.empty

insert :: (Ord k) => [k] -> Trie k -> Trie k
insert [] (Node _ map) = Node True map
insert (x:xs) (Node b map)
  | x `M.member` map = Node b (M.adjust (Trie.insert xs) x map)
  | otherwise = Node b (M.insert x (Trie.insert xs Trie.empty) map)

fromList :: (Ord k) => [[k]] -> Trie k
fromList = foldr Trie.insert Trie.empty

member :: (Ord k) => [k] -> Trie k -> Bool
member [] (Node b _) = b
member (x:xs) (Node _ map) = x `M.member` map && xs `Trie.member` (map M.! x)
