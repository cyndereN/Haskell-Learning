module GameLibrary where

import Data.List as L
import System.Random

data Operation = MoveLeft | MoveUp | MoveRight | MoveDown 

type Board = [[Int]]

startBoard :: Board
startBoard = replicate 4 ( replicate 4 0 )

snoc :: [a] -> a -> [a]
snoc [] x = [x] 
snoc (x:xs) y = x : snoc xs y

moveRowLeft :: [Int] -> [Int]
moveRowLeft = concatRowLeft . moveZeroRight

moveZeroRight :: [Int] -> [Int]
moveZeroRight [] = [] 
moveZeroRight (0:xs) = snoc (moveZeroRight xs) 0
moveZeroRight (x:xs) = x : moveZeroRight xs

concatRowLeft :: [Int] -> [Int]
concatRowLeft [] = []
concatRowLeft [x] = [x]
concatRowLeft (x:y:xs) = if x == y
                       then (x+y):(snoc xs 0)
                       else x:(concatRowLeft (y:xs))

moveBoard :: Operation -> Board -> Board
moveBoard MoveLeft = map moveRowLeft
moveBoard MoveRight = map reverse . map moveRowLeft . map reverse
moveBoard MoveUp = L.transpose . map moveRowLeft. L.transpose
moveBoard MoveDown = L.transpose . moveBoard MoveRight . L.transpose

playerWon :: Board -> Bool
playerWon b = 2048 `elem` (concat b)

set :: Int -> a -> [a] -> [a]
set i e l = take i l ++ [e] ++ drop (i+1) l

placeANEWTwo :: Board -> IO Board
placeANEWTwo b = do 
    i <- randomRIO (0,3)
    j <- randomRIO (0,3)
    if b!!i!!j == 0
        then return $ set i (set j 2 (b !! i)) b
        else placeANEWTwo b

boardMovable :: Board -> Bool
boardMovable b = ( b /= moveBoard MoveLeft b)
               || ( b /= moveBoard MoveUp b)
               || ( b /= moveBoard MoveDown b)
               || ( b /= moveBoard MoveRight b)  