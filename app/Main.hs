module Main (main) where

import System.Random
import System.Environment

import Deathstacks (listMoves)
import Board (buildBoard, Player (Red, Blue))


main :: IO ()
main = do
    args <- getArgs
    let (fen:p:_) = args
        player = if p == "r" then Red else Blue
        moves = listMoves (buildBoard fen) player in
            if (last args) == "-all" then
                putStrLn (show moves)
            else
                do
                    rand <- randomRIO (0, (length moves)-1)
                    putStrLn (show (moves!!rand)) 

