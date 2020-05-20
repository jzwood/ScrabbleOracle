module Main where

import Oracle
import Data.Matrix (fromLists)
import ScrabbleBoard

--newtype Tile = Tile (Char, Integer)
  --deriving (Show, Eq)
--data Bonus = TripleWordScore | DoubleWordScore | TripleLetterScore | DoubleLetterScore
  --deriving (Show, Eq)
--newtype Square = Square (Maybe Tile, Maybe Bonus)
  --deriving (Show, Eq)
--type Board = Matrix Square


eSquare = Square (Nothing, Nothing)

testBoard1 = fromLists
  [
    [eSquare, eSquare,                      eSquare,  eSquare],
    [eSquare, Square (Just (Tile ('r', 0)), Nothing), eSquare],
    [eSquare, Square (Just (Tile ('e', 0)), Nothing), eSquare]
  ]

testRack1 = [Tile ('a', 1), Tile ('c', 1), Tile ('w', 4)]

main :: IO ()
main = do
  res <- oracle testBoard1 testRack1
  Prelude.putStr . show $ res
