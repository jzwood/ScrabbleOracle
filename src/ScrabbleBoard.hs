module ScrabbleBoard where

import Data.Matrix
import Data.Maybe (fromMaybe)
import ScrabbleBoard (Score)

newtype Tile = Tile (Char, Int)
  deriving (Show, Eq)
data Bonus = TripleWordScore | DoubleWordScore | TripleLetterScore | DoubleLetterScore
  deriving (Show, Eq)
newtype Square = Square (Maybe Tile, Maybe Bonus)
  deriving (Show, Eq)
type Board = Matrix Square

newtype TileCoordinate = Coordinate (Int, Int)
  deriving (Show, Eq)
type WordPlacement = [TileCoordinate]

type Rack = String
type PlaySpot = (String, [WordPlacement])

coordinateToSquare :: Board -> TileCoordinate -> Maybe Square
coordinateToSquare board (Coordinate (x, y)) = safeGet x y board

unsafeCoordinateToSquare :: Board -> TileCoordinate -> Square
unsafeCoordinateToSquare board (Coordinate (x, y)) = getElem x y board

squareToTile :: Square -> Tile
squareToTile (Square (tile, _)) = fromMaybe (Tile ('_', 0)) tile

tileToChar :: Tile -> Char
tileToChar (Tile (c, _)) = c

hasChar :: Maybe Square -> Bool
hasChar (Just (Square (Just (Tile _), _))) = True
hasChar _ = False

hasBonus :: Maybe Square -> Bool
hasBonus (Just (Square (_, Just _))) = True
hasBonus _ = False

boardSize = 15

charToValue = [ ('*', 0),
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
                ('Z', 10) ]
