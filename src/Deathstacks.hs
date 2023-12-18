module Deathstacks where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board


-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, steps :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ show startR ++ "-" ++ show tr ++ "-" ++ [tarC] ++ show tarR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 


-- #############################################################################
-- #################### playerWon :: Board -> Maybe Player #####################
-- #################### - 4 Functional Points              #####################
-- #################### - 1 Coverage Point                 #####################
-- #############################################################################

playerWon :: Board -> Maybe Player
playerWon _ = Just Red


-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 4 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves _ _ = []


-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

isValidMove :: Board -> Move -> Bool
isValidMove _ _ = False
  

-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves _ _ = [Move (Pos 'a' 1) (Pos 'a' 1) 10]
