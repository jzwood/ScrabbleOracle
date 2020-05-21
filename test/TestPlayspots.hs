module TestPlayspots where

import Playspots
import TestData
import Discovery
import ScrabbleBoard
import qualified Trie as T


spots = legalPlayspotCoords testBoard1
frags = groupCoordsByFragment $ map (getFragment testBoard1) spots
strs = fillFrags (toLetters testRack1) frags

trie = T.fromList frags


main :: IO ()
--main = Prelude.putStr . show $ map fst strs
main = Prelude.putStr . show $ map fst frags
