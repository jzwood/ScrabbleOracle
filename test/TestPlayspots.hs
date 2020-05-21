module TestPlayspots where

import Playspots
import TestData
import Discovery
import ScrabbleBoard
import qualified Trie as T


spots = legalPlayspotCoords testBoard1
frags = groupCoordsByFragment $ map (getFragment testBoard1) spots
strs = fillFrags (toLetters testRack1) frags

{-
isWord <- isInDictionary
x1 = filter (isWord . fst) strs
x2 = expandPlayspots x1
x3 = map fst $ concatMap (getCrossPlayspots testBoard1) x2
x4 = filter isWord x3
-}

trie = T.fromList frags


main :: IO [(String, Coords)]
main = do
--main = Prelude.putStr . show $ map fst strs
  x <- validateGroupedPlayspots testBoard1 strs
  Prelude.putStr . show $ x
  return x
