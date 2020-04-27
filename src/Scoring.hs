module Scoring where

import ScrabbleBoard

type Score = Integer

placementToScore :: Board -> String -> WordPlacement -> Score
placementToScore = undefined

tileToScore :: Tile -> Score
tileToScore (Tile (a, s)) = s

scoreWord :: [Square] -> Score
scoreWord (Square (Nothing, _)) = 0
