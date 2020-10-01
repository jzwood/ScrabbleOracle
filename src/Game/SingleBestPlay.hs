module Game.SingleBestPlay where

import AI.Oracle
import Data.List
import Game.ScrabbleBoard
import Game.ScrabbleGame
import qualified Data.Map as M
import qualified Data.Matrix as Mat

--parseForeignBoardRepresentation :: String -> Maybe Board
--parseForeignBoardRepresentation rawboard =
  --where
    --twoDimBoard = lines rawBoard

rawBoardToBoard :: [String] -> Board
rawBoardToBoard rawBoard = Mat.fromLists $ map (map rawCharToSquare) rawBoard

applyPlayspot :: Board -> [(Char, TileCoordinate)] -> Board
applyPlayspot = foldr (\ (c, Coordinate coord) b -> Mat.unsafeSet (Square (Just (Tile (c, charToValue M.! c)), Nothing)) coord b)

makeSinglePlay :: Board -> Rack -> IO (Board, Score)
makeSinglePlay board rack =
  if genericLength rack == 0
    then return (board, 0)
  else do
    rankedPlayspots <- oracle board rack
    if genericLength rankedPlayspots == 0
      then return (board, 0)
    else do
      let (word, score, coords) = head rankedPlayspots
          charCoords = zip word coords
          newBoard = applyPlayspot board charCoords
      return (newBoard, score)
