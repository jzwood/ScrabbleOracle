module TestPlayspots where

import Playspots
import TestData
import Discovery
import ScrabbleBoard
import qualified Trie as T


spots = legalPlayspotCoords testBoard1
frags = groupCoordsByFragment $ map (getFragment testBoard1) spots
strs = fillFrags (toLetters testRack1) frags

spots2 = legalPlayspotCoords testBoard2
frags2 = groupCoordsByFragment $ map (getFragment testBoard2) spots2
strs2 = fillFrags (toLetters testRack2) frags2

spots3 = legalPlayspotCoords testBoard3
frags3 = groupCoordsByFragment $ map (getFragment testBoard3) spots3
strs3 = fillFrags (toLetters testRack3) frags3

{-
isWord <- isInDictionary
x1 = filter (isWord . fst) strs
x2 = expandPlayspots x1
x3 = map fst $ concatMap (getCrossPlayspots testBoard1) x2
x4 = filter isWord x3
-}
{-
isWord <- isInDictionary
y1 = filter (isWord . fst) strs2
y2 = expandPlayspots y1
y3 = map fst $ concatMap (getCrossPlayspots testBoard2) y2
y4 = filter isWord y3
-}
{-
isWord <- isInDictionary
z1 = filter (isWord . fst) strs3
z2 = expandPlayspots z1
z3 = map fst $ concatMap (getCrossPlayspots testBoard2) z2
z4 = filter isWord z3
-}

trie = T.fromList frags

--getCrossPlayspot :: Board -> (Int, Int) -> Char -> TileCoordinate -> (String, [Square])

main :: IO ()
main = do
--main = Prelude.putStr . show $ map fst strs
  x <- validateGroupedPlayspots testBoard1 strs
  Prelude.putStr . show $ map fst x
  y <- validateGroupedPlayspots testBoard2 strs2
  Prelude.putStr . show $ map fst y
  z <- validateGroupedPlayspots testBoard3 strs3
  Prelude.putStr . show $ map fst z
  return ()
