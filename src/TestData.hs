module TestData where

import Data.Matrix (fromLists)
import Game.ScrabbleBoard

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

testRack1 = "CAW"
testRack2 = "XI"
testRack3 = "OI"

