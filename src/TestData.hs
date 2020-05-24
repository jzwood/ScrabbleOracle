module TestData where

import Data.Matrix (fromLists)
import ScrabbleBoard

eSquare = Square (Nothing, Nothing)

testBoard1 = fromLists
  [
    [eSquare, eSquare,                                                eSquare],
    [eSquare, Square (Nothing, Just DoubleWordScore),                 eSquare],
    [eSquare, Square (Just (Tile ('R', 1)), Just DoubleLetterScore),  eSquare],
    [eSquare, Square (Just (Tile ('E', 1)), Nothing),                 eSquare]
  ]

testBoard2 = fromLists
  [
    [Square (Just (Tile ('A', 1)), Nothing), eSquare],
    [Square (Just (Tile ('M', 3)), Nothing), eSquare]
  ]

testBoard3 = fromLists
  [
    [Square (Just (Tile ('N', 1)), Nothing), eSquare],
    [Square (Just (Tile ('X', 8)), Nothing), eSquare]
  ]

testRack1 = [Tile ('A', 1), Tile ('C', 1), Tile ('W', 4)]
testRack2 = [Tile ('X', 1), Tile ('I', 1)]
testRack3 = [Tile ('O', 1), Tile ('I', 1)]

