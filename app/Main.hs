module Main where

import AI.Oracle
import Data.Matrix (fromLists)
import Game.ScrabbleBoard
import Game.BotVsBot
import TestData

main :: IO ()
--main = do
  --res <- oracle testBoard1 testRack1
  --Prelude.putStr . show $ res
main = playGame
