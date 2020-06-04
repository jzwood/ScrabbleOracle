module BotVsBot where


import AI.Oracle
import Data.List
import Game.ScrabbleBoard
import Game.ScrabbleGame
import System.Random
import qualified Data.Matrix as Mat
import qualified Data.Map as M


type LetterBag = String
--oracle :: Board -> Rack -> IO [(String, Score, Coords)]

applyPlayspot :: Board -> [(Char, TileCoordinate)] -> Board
applyPlayspot = foldr (\ (c, Coordinate coord) b -> Mat.unsafeSet (Square (Just (Tile (c, charToValue M.! c)), Nothing)) coord b)

drawFromLetterBag :: LetterBag -> Rack -> Board -> [(Char, TileCoordinate)] -> (LetterBag, Rack)
drawFromLetterBag letterBag rack board charCoords = (newLetterBag, newRack)
  where
    playedLetters = map fst $ filter (not . hasChar . coordinateToSquare board . snd) charCoords
    numLettersPlayed = genericLength playedLetters
    newRack = (rack \\ playedLetters) ++ take numLettersPlayed letterBag
    newLetterBag = drop numLettersPlayed letterBag

makePlay :: LetterBag -> Board -> Rack -> IO (Maybe (LetterBag, Board, Rack, Score))
makePlay letterBag board rack =
  if genericLength rack == 0
    then return Nothing
  else do
    rankedPlayspots <- oracle board rack
    if genericLength rankedPlayspots == 0
      then return Nothing
    else do
      let (word, score, coords) = head rankedPlayspots
          charCoords = zip word coords
          newBoard = applyPlayspot board charCoords
          (newLetterBag, newRack) = drawFromLetterBag letterBag rack board charCoords
      return (Just (newLetterBag, newBoard, newRack, score))


playGame :: StdGen -> IO (Maybe Board)
playGame g = do
  let letterBag = initLetterBag g
      (p1Rack, letterBag1) = genericSplitAt 7 letterBag
      (p2Rack, letterBag2) = genericSplitAt 7 letterBag1
  play <- makePlay letterBag1 startBoard p1Rack
  case play of
    Nothing -> return Nothing
    Just (l, b , r, s) -> return (Just b)
