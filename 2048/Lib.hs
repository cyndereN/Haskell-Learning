module Lib where

import System.IO
import GameLibrary
import Data.List as L


gameStart :: IO()
gameStart = return startBoard >>= placeANEWTwo >>= placeANEWTwo >>= loop

loop :: Board -> IO()
loop currentBoard = do 
    printBoard currentBoard
    s <- getLine
    case convertToOperation s of
        Nothing -> putStrLn "Unrecognized Command" >> loop currentBoard
        Just op -> do
            let afterMoveBoard = moveBoard op currentBoard
            if playerWon afterMoveBoard
                then putStrLn "You won!" >> return ()
                else if afterMoveBoard == currentBoard
                    then loop currentBoard
                    else do
                        newBoard <- placeANEWTwo afterMoveBoard
                        if boardMovable newBoard
                            then loop newBoard
                            else do
                                putStrLn "" 
                                printBoard newBoard 
                                putStrLn "You lost!" 
                                return ()

convertToOperation :: String -> Maybe Operation
convertToOperation ('j': _) = return MoveLeft
convertToOperation ('k': _) = return MoveDown
convertToOperation ('i': _) = return MoveUp
convertToOperation ('l': _) = return MoveRight
convertToOperation _ = Nothing

printBoard :: Board -> IO()
printBoard b = putStrLn(unlines $ map(concat.L.intersperse"\t".map show) b)