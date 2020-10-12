module Game.ScrabbleBoard where

import Control.Applicative
import Data.Char
import Data.List
import Data.Matrix
import Data.Maybe (fromMaybe)
import System.Random
import qualified Data.Map as Map
import qualified Data.Matrix as Mat

-- BOARD DATA STRUCTURE

newtype Tile = Tile (Char, Integer)
  deriving (Show, Eq)
data Bonus = TripleWordScore | DoubleWordScore | TripleLetterScore | DoubleLetterScore
  deriving (Show, Eq)
newtype Square = Square (Maybe Tile, Maybe Bonus)
  deriving (Show, Eq)
type Board = Matrix Square

newtype TileCoordinate = Coordinate (Int, Int)
  deriving (Show, Eq)

type Coords = [TileCoordinate]
type WordFragment = String
type Rack = String
type Score = Integer

nullBoard :: Board
nullBoard = Mat.fromList 0 0 []

-- CONSTANTS

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

mysteryChar = '?'

blank = '_'
charToValue = Map.fromList [('_', 0),
                          ('A', 1),
                          ('B', 3),
                          ('C', 3),
                          ('D', 2),
                          ('E', 1),
                          ('F', 4),
                          ('G', 2),
                          ('H', 4),
                          ('I', 1),
                          ('J', 8),
                          ('K', 5),
                          ('L', 1),
                          ('M', 3),
                          ('N', 1),
                          ('O', 1),
                          ('P', 3),
                          ('Q', 10),
                          ('R', 1),
                          ('S', 1),
                          ('T', 1),
                          ('U', 1),
                          ('V', 4),
                          ('W', 4),
                          ('X', 8),
                          ('Y', 4),
                          ('Z', 10)]

baseBoard :: [String]
baseBoard =
  [ "4__1___4___1__4"
  , "_2___3___3___2_"
  , "__2___1_1___2__"
  , "1__2___1___2__1"
  , "____2_____2____"
  , "_3___3___3___3_"
  , "__1___1_1___1__"
  , "4__1___2___1__4"
  , "__1___1_1___1__"
  , "_3___3___3___3_"
  , "____2_____2____"
  , "1__2___1___2___"
  , "__2___1_1___2__"
  , "_2___3___3___2_"
  , "4__1___4___1__4"
  ]


botStrBoard :: [String]
botStrBoard =
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

botStartBoard :: Board
botStartBoard = Mat.fromLists $ map (map charToSquare) botStrBoard

-- BOARD MANIPULATION UTILITIES

coordinateToSquare :: Board -> TileCoordinate -> Maybe Square
coordinateToSquare board (Coordinate (x, y)) = safeGet x y board

toVector :: TileCoordinate -> TileCoordinate -> (Int, Int)
toVector (Coordinate (x1, y1)) (Coordinate (x2, y2)) = (x2 - x1, y2 - y1)

unsafeCoordinateToSquare :: Board -> TileCoordinate -> Square
unsafeCoordinateToSquare board (Coordinate (x, y)) = getElem x y board

squareToTile :: Char -> Square -> Tile
squareToTile defaultChar (Square (tile, _)) = fromMaybe (Tile (defaultChar, 0)) tile

tileToChar :: Tile -> Char
tileToChar (Tile (c, _)) = c

hasChar :: Maybe Square -> Bool
hasChar (Just (Square (Just (Tile _), _))) = True
hasChar _ = False

unsafeHasChar :: Square -> Bool
unsafeHasChar (Square (Just (Tile _), _)) = True
unsafeHasChar _ = False

hasBonus :: Maybe Square -> Bool
hasBonus (Just (Square (_, Just _))) = True
hasBonus _ = False

easyReadBoard :: Board -> Mat.Matrix Char
easyReadBoard = Mat.mapPos (\_ s -> squareToChar s)
  where
    squareToChar :: Square -> Char
    squareToChar (Square (Nothing, _)) = ' '
    squareToChar (Square (Just (Tile (c, _)), _)) = c

initLetterBag :: StdGen -> String
initLetterBag stdGen = shuffledLetters
  where
    orderedLetters = concatMap (\(c, n) -> genericReplicate n c) charFrequencies
    randIntStream = randomRs (0::Integer, 100 * 25::Integer) stdGen
    taggedLetters = zip randIntStream orderedLetters
    shuffledTaggedLetters = sortOn fst taggedLetters
    shuffledLetters = map snd shuffledTaggedLetters

charToSquare :: Char -> Square
charToSquare c = case c of
  '_' -> Square (Nothing, Nothing)
  '1' -> Square (Nothing, Just DoubleLetterScore)
  '2' -> Square (Nothing, Just DoubleWordScore)
  '3' -> Square (Nothing, Just TripleLetterScore)
  '4' -> Square (Nothing, Just TripleWordScore)
  _   -> Square (Just (Tile (c, Map.findWithDefault 1 c charToValue)), Nothing)

-- PRESENTATION UTILS

printPlay :: Board -> String -> Score -> IO ()
printPlay board word score = do
    putStr $ genericReplicate 100 '\n'
    putStr . prettyPrint $ board
    putChar '\n'
    putStrLn $ "word: " ++ word
    putStrLn $ "score: " ++ show score

-- GAME UTILS

applyPlayspot :: Board -> [(Char, TileCoordinate)] -> Board
applyPlayspot = foldr (\ (c, Coordinate coord) b -> Mat.unsafeSet (Square (Just (Tile (c, charToValue Map.! c)), Nothing)) coord b)

-- PARSING FOREIGN INPUT

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&) f1 f2 a = f1 a && f2 a

(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) f1 f2 a = f1 a || f2 a

parseBoard :: String -> Maybe Board
parseBoard strBoard =
  if
     isFullBoard .&& all (isAllowedChar .|| isNoChar) .&& (not . all isNoChar) $ strBoard
  then
    Just $ Mat.fromList 15 15 $ map charToSquare overlayBaseBoard
  else
    Nothing
  where
    isFullBoard :: String -> Bool
    isFullBoard xs = genericLength xs == 15^2
    isNoChar :: Char -> Bool
    isNoChar = (=='_')
    isAllowedChar :: Char -> Bool
    isAllowedChar = isAlpha .&& isUpper
    chooseChar :: Char -> Char -> Char
    chooseChar inputChar baseChar = if isNoChar inputChar then baseChar else inputChar
    overlayBaseBoard :: String
    overlayBaseBoard = zipWith chooseChar strBoard (concat baseBoard)

parseRack :: String -> Maybe Rack
parseRack strRack =
  if
    genericLength strRack == 7 && all (isAlpha .&& isUpper) strRack
  then
    Just strRack
  else
    Nothing

stringifyBoard :: Board -> String
stringifyBoard board = unlines . Mat.toLists $ easyReadBoard board

prettyPrint :: Board -> String
prettyPrint board =
  horizontalBorder ++
  (unlines . map verticalBorder $ Mat.toLists $ easyReadBoard board) ++
  horizontalBorder
    where horizontalBorder = '+' : replicate 15 '-' ++ "+\n"
          verticalBorder row = '|' : row ++ "|"
