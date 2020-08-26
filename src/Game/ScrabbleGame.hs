module Game.ScrabbleGame where

import Data.List
import Data.Matrix as M
import Game.ScrabbleBoard
import System.Random
import qualified Data.Map as Map


rawStartBoard :: [String]
rawStartBoard =
  [ "4__1___4___1__4"
  , "_2___3___3___2_"
  , "__2___1_1___2__"
  , "1__2___1___2__1"
  , "____2_____2____"
  , "_3___3___3___3_"
  , "__1___1_1___1__"
  , "4__1___A___1__4"
  , "__1___1_1___1__"
  , "_3___3___3___3_"
  , "____2_____2____"
  , "1__2___1___2___"
  , "__2___1_1___2__"
  , "_2___3___3___2_"
  , "4__1___4___1__4"
  ]


rawCharToSquare :: Char -> Square
rawCharToSquare c = case c of
  '_' -> Square (Nothing, Nothing)
  '1' -> Square (Nothing, Just DoubleLetterScore)
  '2' -> Square (Nothing, Just DoubleWordScore)
  '3' -> Square (Nothing, Just TripleLetterScore)
  '4' -> Square (Nothing, Just TripleWordScore)
  _   -> Square (Just (Tile (c, Map.findWithDefault 1 c charToValue)), Nothing)

startBoard :: Board
startBoard = M.fromLists $ map (map rawCharToSquare) rawStartBoard

squareToChar :: Square -> Char
squareToChar (Square (Nothing, _)) = ' '
squareToChar (Square (Just (Tile (c, _)), _)) = c

--boardToRawString :: Board -> [String]
--boardToRawString board = map (map squareToChar) $ M.toLists board

easyReadBoard :: Board -> M.Matrix Char
easyReadBoard = M.mapPos (\_ s -> squareToChar s)

--maybeBoardToRawString :: Maybe Board -> [String]
--maybeBoardToRawString Nothing = []
--maybeBoardToRawString (Just b) = boardToRawString b

initLetterBag :: StdGen -> String
initLetterBag stdGen = shuffledLetters
  where
    orderedLetters = concatMap (\(c, n) -> genericReplicate n c) charFrequencies
    randIntStream = randomRs (0::Integer, 100 * 25::Integer) stdGen
    taggedLetters = zip randIntStream orderedLetters
    shuffledTaggedLetters = sortOn fst taggedLetters
    shuffledLetters = map snd shuffledTaggedLetters

charFrequencies :: [(Char, Integer)]
charFrequencies =
  [ ('A', 9)
  , ('B', 2)
  , ('C', 2)
  , ('D', 4)
  , ('E', 12)
  , ('F', 2)
  , ('G', 3)
  , ('H', 2)
  , ('I', 9)
  , ('J', 1)
  , ('K', 1)
  , ('L', 4)
  , ('M', 2)
  , ('N', 6)
  , ('O', 8)
  , ('P', 2)
  , ('Q', 1)
  , ('R', 6)
  , ('S', 4)
  , ('T', 6)
  , ('U', 4)
  , ('V', 2)
  , ('W', 2)
  , ('X', 1)
  , ('Y', 2)
  , ('Z', 1)
  ]
