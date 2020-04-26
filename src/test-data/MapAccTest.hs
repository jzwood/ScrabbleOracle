module MapAccTest where

import Data.List

groupCandidates :: [(Char, Integer)] -> [(Char, [Integer])]
groupCandidates candidates = map fold' $ group' $ sort' candidates
  where
    sort' = sortOn fst
    group' = groupBy (\(a, _) (b, _) -> a == b)
    fold' = foldr (\(k, v) (_, v') -> (k, v : v')) ('_', [])

--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
testList = [('a', 3), ('b', 4), ('a', 4), ('c', 2), ('b', 1)]
--[('a', [3, 4], ('b', [4]))]

res = 4

main :: IO ()
main = do
  putStr . show $ groupCandidates testList
