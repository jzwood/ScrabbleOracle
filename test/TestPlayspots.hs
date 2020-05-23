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

trie = T.fromList frags

--getCrossPlayspot :: Board -> (Int, Int) -> Char -> TileCoordinate -> (String, [Square])

main :: IO ()
main = do
--main = Prelude.putStr . show $ map fst strs
  x <- validateGroupedPlayspots testBoard1 strs
  Prelude.putStr . show $ map fst x
  y <- validateGroupedPlayspots testBoard2 strs2
  Prelude.putStr . show $ map fst y
  return ()
