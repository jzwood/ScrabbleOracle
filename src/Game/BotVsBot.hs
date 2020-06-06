module BotVsBot where


import AI.Oracle
import Control.Monad.Loops
import Data.List
import Game.ScrabbleBoard
import Game.ScrabbleGame
import System.Random
import qualified Data.Map as M
import qualified Data.Matrix as Mat


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

makePlay :: (LetterBag, Board, Rack) -> IO (LetterBag, Board, Rack)
makePlay (letterBag, board, rack) =
  if genericLength rack == 0
    then return (letterBag, board, rack)
  else do
    rankedPlayspots <- oracle board rack
    if genericLength rankedPlayspots == 0
      then return (letterBag, board, rack)
    else do
      let (word, score, coords) = head rankedPlayspots
          charCoords = zip word coords
          newBoard = applyPlayspot board charCoords
          (newLetterBag, newRack) = drawFromLetterBag letterBag rack board charCoords
      return (newLetterBag, newBoard, newRack)

startGame :: StdGen -> IO (LetterBag, Board, Rack)
startGame g =
  let
    letterBag = initLetterBag g
    (p1Rack, letterBag1) = genericSplitAt 7 letterBag
    (p2Rack, letterBag2) = genericSplitAt 7 letterBag1
  in makePlay (letterBag1, startBoard, p1Rack)


playGame :: IO ()
playGame = do
  g <- newStdGen
  (l, b, r) <- startGame g >>= makePlay >>= makePlay >>= makePlay >>= makePlay
  putStr . show $ easyReadBoard b

playGame' :: IO ()
playGame' = do
  g <- newStdGen
  game@(l, b, r) <- startGame g
  game@(l, b, r) <- makePlay game
  game@(l, b, r) <- makePlay game
  game@(l, b, r) <- makePlay game
  putStr . show $ easyReadBoard b

isGameOver :: (LetterBag, Board, Rack) -> Bool
isGameOver (l, _, _) = genericLength l == 0

playGame'' :: IO ()
playGame'' = do
  g <- newStdGen
  start <- startGame g
  (_, b, _) <- iterateUntilM isGameOver makePlay start
  putStr . show $ easyReadBoard b

main :: IO ()
main = playGame''
