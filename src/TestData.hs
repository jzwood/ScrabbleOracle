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
    [Square (Just (Tile ('N', 0)), Nothing), eSquare],
    [Square (Just (Tile ('X', 0)), Nothing), eSquare]
  ]
  {-
      NO    NI
      XI    XO
  -}

testRack1 = [Tile ('A', 1), Tile ('C', 1), Tile ('W', 4)]
testRack2 = [Tile ('O', 1), Tile ('I', 1)]

