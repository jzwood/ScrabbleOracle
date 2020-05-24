module Spec where

import Test.Hspec
import Data.List
import qualified Data.Matrix as M

import Playspots
import Discovery
import ScrabbleBoard

es = Square (Nothing, Nothing)
s :: Char -> Score -> Square
s c v = Square (Just (Tile (c, v)), Nothing)

testBoard1 = M.matrix 9 9 $ \(i,j) -> if i == 4 && j == 4 then s 'A' 1 else es
testRack1 = "BCD"
output1 = []

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

expected str = nub $ filter ((>1) . genericLength) $ sort (concatMap permutations $ powerset str)

main :: IO ()
main = hspec $ do
  describe "fillFrags strings" $ do
    it "returns all anagrams" $
      fragStrings testBoard1 testRack1 `shouldBe` expected "ABCD"

    --it "returns a positive number when given a negative input" $
      --absolute (-1) `shouldBe` 1

    --it "returns zero when given zero" $
      --absolute 0 `shouldBe` 0
