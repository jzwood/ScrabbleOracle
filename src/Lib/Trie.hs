module Lib.Trie where

import Data.Map (Map)
import qualified Data.Map.Lazy as M

-- [k] can be used to look up v
data Trie k v = Node (Maybe v) (Map k (Trie k v))
  deriving (Show, Eq)

empty :: (Ord k) => Trie k v
empty = Node Nothing M.empty

null :: (Ord k, Eq v) => Trie k v -> Bool
null = (==Lib.Trie.empty)

insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insert [] v (Node _ map) = Node (Just v) map
insert (x:xs) v (Node v' map)
  | x `M.member` map = Node v' (M.adjust (Lib.Trie.insert xs v) x map)
  | otherwise = Node v' (M.insert x (Lib.Trie.insert xs v Lib.Trie.empty) map)

fromList :: (Ord k) => [([k], v)] -> Trie k v
fromList = foldr (uncurry Lib.Trie.insert) Lib.Trie.empty

member :: (Ord k) => [k] -> Trie k v -> Bool
member [] (Node Nothing _) = False
member [] (Node _ _) = True
member (x:xs) (Node _ map) = x `M.member` map && xs `Lib.Trie.member` (map M.! x)
