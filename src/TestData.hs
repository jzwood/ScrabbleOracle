module TestData where

import Data.Matrix (fromLists)
import ScrabbleBoard

eSquare = Square (Nothing, Nothing)

testBoard1 = fromLists
  [
    [eSquare, eSquare,                                eSquare],
    [eSquare, eSquare,                                eSquare],
    [eSquare, Square (Just (Tile ('R', 0)), Nothing), eSquare],
    [eSquare, Square (Just (Tile ('E', 0)), Nothing), eSquare]
  ]

testBoard2 = fromLists
  [
    [Square (Just (Tile ('A', 0)), Nothing), eSquare],
    [Square (Just (Tile ('M', 0)), Nothing), eSquare]
  ]

testBoard3 = fromLists
  [
    [Square (Just (Tile ('N', 0)), Nothing), eSquare],
    [Square (Just (Tile ('X', 0)), Nothing), eSquare]
  ]

testRack1 = [Tile ('A', 1), Tile ('C', 1), Tile ('W', 4)]
testRack2 = [Tile ('X', 1), Tile ('I', 1)]
testRack3 = [Tile ('O', 1), Tile ('I', 1)]

