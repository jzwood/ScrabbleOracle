module Playground where

import Data.List (nub)
import qualified Data.Matrix as M

import AI.Playspots
import AI.Discovery
import Game.ScrabbleBoard

es = Square (Nothing, Nothing)
s :: Char -> Score -> Square
s c v = Square (Just (Tile (c, v)), Nothing)

testBoard1 = M.matrix 15 15 $ \(i, j) -> if i == 8 && j == 8 then s 'E' 1 else es
r1 = "STREAMS"

--testRacks =
  --[
    --"AWI",
    --"POIHDUH",
    --"IEHHDUB",
    --"STREAMS"
  --]


fragStrings :: Board -> Rack -> [String]
fragStrings testBoard testRack = nub $ map fst strs
  where
    spots = legalPlayspotCoords testBoard
    frags = groupCoordsByFragment $ map (getFragment testBoard) spots
    strs = fillFrags testRack frags

main :: IO ()
--main = putStr . show $ fragStrings testBoard1 r1
main = putStr . show $ startBoard
