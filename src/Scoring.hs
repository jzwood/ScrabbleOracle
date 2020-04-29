module Scoring where

import ScrabbleBoard

score :: Board -> (String, PlaySpot) -> (PlaySpot, Score)
score = undefined

tileToScore :: Tile -> Score
tileToScore (Tile (a, s)) = s

scoreWord :: [Square] -> Score
scoreWord = undefined
