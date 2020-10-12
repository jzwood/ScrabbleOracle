module TestSingleBestPlay where

import Game.SingleBestPlay
import AI.Oracle
import Control.Monad.Loops
import Data.List
import Game.ScrabbleBoard
import System.Random
import qualified Data.Map as M
import qualified Data.Matrix as Mat


rawStartBoard :: [String]
rawStartBoard =
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


rawBoard :: [String]
rawBoard =
  [ "S__MINER___1__O"
  , "ASHY_3___G_PINK"
  , "R_A___1_1LEAF_A"
  , "I_JUCO_1GOWN__1"
  , "____O___ROE____"
  , "_3__IF__UM___3_"
  , "__1_RID_N___1__"
  , "4__1_VENT__1__4"
  , "__Q_DEX_1___1__"
  , "_VAPER___ZAGS3_"
  , "__T_B____O2____"
  , "1__HO_ABEAM2___"
  , "__LINTY_1___2__"
  , "_2__EE___3___2_"
  , "OILED__4___1__4"
  ]

rack :: String
rack = "CREATES"


rawBoard2 :: [String]
rawBoard2 =
  [ "4__1___N___1__4"
  , "JOULES_A___QAT_"
  , "OY2__T1R1___B__"
  , "1__2_A_CONE2O_1"
  , "_RUDER___ADORN_"
  , "_3___L_PIG__T3_"
  , "_V1__I1U1___I__"
  , "MILLINER___1OOF"
  , "_V1__G1G1___N__"
  , "_A___3_E_3___3_"
  , "___NEXUS__2____"
  , "1_MAR__1___2___"
  , "__E___1_1___2__"
  , "_2W__3___3___2_"
  , "WAS1___4___1__4"
  ]

rack2 :: String
rack2 = "OZETRIC"

rawBoard3 :: [String]
rawBoard3 =
  [ "4__B___N___1__4"
  , "JOULES_A___QAT_"
  , "OY2E_T1R1___B__"
  , "1__E_A_CONE2O_1"
  , "_RUDER___ADORN_"
  , "_3___L_PIG__T3_"
  , "_V1__I1U1___I__"
  , "MILLINER___1OOF"
  , "_V1__G1G1___NO_"
  , "_A___3_E_3___Z_"
  , "___NEXUS__2__E_"
  , "1_MAT__1___2_D_"
  , "__E___1_1___2__"
  , "_2W__3___3___2_"
  , "WAS1___4___1__4"
  ]

rack3 :: String
rack3 = "FEASTTH"

rawBoard4 :: [String]
rawBoard4 =
  [ "4__1___4___1__4"
  , "_2___3___3___2_"
  , "__2___1_1FOOT__"
  , "1__2___1_L_ROBS"
  , "____2____O2____"
  , "_3___3_L_G___3_"
  , "__1___1A1G__1__"
  , "4__1___PRIM1__4"
  , "__1___1_1N__1__"
  , "_3___3___G___3_"
  , "____2_____2____"
  , "1__2___1___2___"
  , "__2___1_1___2__"
  , "_2___3___3___2_"
  , "4__1___4___1__4"
  ]

rack4 :: String
rack4 = "OZSNFRE"

board :: Board
board = rawBoardToBoard rawBoard3

main :: IO ()
main = do
  (board, word, score) <- makeSinglePlay (rawBoardToBoard rawBoard4) rack4
  putStrLn . prettyPrint $ board
  putStrLn $ "Points: " ++ show score
