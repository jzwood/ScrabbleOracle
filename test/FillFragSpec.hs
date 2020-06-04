module Spec where

import Test.Hspec
import Data.List
import qualified Data.Matrix as M

import AI.Playspots
import AI.Discovery
import Game.ScrabbleBoard

es = Square (Nothing, Nothing)
s :: Char -> Score -> Square
s c v = Square (Just (Tile (c, v)), Nothing)

testBoard1 = M.matrix 9 9 $ \(i,j) -> if i == 4 && j == 4 then s 'A' 0 else es
testRack1 = "BCD"
output1 = permutePowerSets "ABCD"

testBoard2 = M.fromLists
  [
    [Square (Just (Tile ('A', 0)), Nothing), Square (Nothing, Nothing)],
    [Square (Just (Tile ('B', 0)), Nothing), Square (Nothing, Nothing)]
  ]
testRack2 = "CD"
output2 = ["AC","AD","BC","BD","CD","DC"]

-- constructed to test behavior of fillFrags function
fragStrings :: Board -> Rack -> [String]
fragStrings testBoard testRack = sort . nub $ map fst strs
  where
    spots = legalPlayspotCoords testBoard
    frags = groupCoordsByFragment $ map (getFragment testBoard) spots
    strs = fillFrags testRack frags

-- stolen from stackoverflow to test fillFrags
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

permutePowerSets str = nub $ filter ((>1) . genericLength) $ sort (concatMap permutations $ powerset str)

main :: IO ()
main = hspec $
  describe "fillFrags strings" $ do
    it "returns all anagrams" $
      fragStrings testBoard1 testRack1 `shouldBe` output1

    it "returns correct words" $
      fragStrings testBoard2 testRack2  `shouldBe` output2
