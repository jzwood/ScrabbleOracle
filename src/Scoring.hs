module Scoring where

import ScrabbleBoard (Board, WordPlacement)

type Score = Integer

scoreWordPlacement :: WordPlacement -> Board -> Score
scoreWordPlacement _ _ = 9
