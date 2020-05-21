module Main where

import Oracle
import Data.Matrix (fromLists)
import ScrabbleBoard
import TestData

--newtype Tile = Tile (Char, Integer)
  --deriving (Show, Eq)
--data Bonus = TripleWordScore | DoubleWordScore | TripleLetterScore | DoubleLetterScore
  --deriving (Show, Eq)
--newtype Square = Square (Maybe Tile, Maybe Bonus)
  --deriving (Show, Eq)
--type Board = Matrix Square

main :: IO ()
main = do
  res <- oracle testBoard1 testRack1
  Prelude.putStr . show $ res
