module TestData where

import Data.Matrix (fromLists)
import ScrabbleBoard

eSquare = Square (Nothing, Nothing)

testBoard1 = fromLists
  [
    [eSquare, eSquare,                                eSquare],
    [eSquare, eSquare,                                eSquare],
    [eSquare, Square (Just (Tile ('r', 0)), Nothing), eSquare],
    [eSquare, Square (Just (Tile ('e', 0)), Nothing), eSquare]
  ]

testRack1 = [Tile ('a', 1), Tile ('c', 1), Tile ('w', 4)]
