module Game.SingleBestPlay where

import AI.Oracle
import Data.List
import Game.ScrabbleBoard
import qualified Data.Map as Map
import qualified Data.Matrix as Mat

rawBoardToBoard :: [String] -> Board
rawBoardToBoard rawBoard = Mat.fromLists $ map (map charToSquare) rawBoard

makeSinglePlay :: Board -> Rack -> IO (Board, String, Score)
makeSinglePlay board [] = return (board, "", 0)
makeSinglePlay board rack = do
    rankedPlayspots <- oracle board rack
    case rankedPlayspots of
      [] -> return (board, "", 0)
      (best:others) -> do
        let (word, score, coords) = best
            charCoords = zip word coords
            newBoard = applyPlayspot board charCoords
        return (newBoard, word, score)

--ForeignFunctionStrBoardStrRackToMakePlay :: String -> String -> IO (String, String, Integer)
--ForeignFunctionStrBoardStrRackToMakePlay strBoard strRack = do
  --(board, word, score) <- liftA2 makeSinglePlay (parseBoard strBoard) (parseRack strRack)
