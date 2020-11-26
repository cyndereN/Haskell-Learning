square :: Int->Int
square x = x * x

pyth :: (Int,Int)->Int
pyth (x,y) = square x + square y

isTriple a b c = if square c == pyth(a, b) then True else False

isTripleAny = isTriple a b c || isTriple a c b || isTriple c b a 

{-
isTripleAny a b c 
  | isTriple a b c = True
  | isTriple a c b = True
  | isTriple c b a = True 
  | otherwise = False -}

halfEvens :: [Int] -> [Int]
halfEvens    [] = []
halfEvens (x:lst) = (if x `mod` 2 == 0 then x `div` 2  else x) : halfEvens lst

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = [ x | x <- xs, x >= a && x <= b ]

positives xs = [x | x <- xs, x > 0]
countPositives []     = 0
countPositives xs = length(positives xs)

import qualified Data.Char as Char
capitalised :: String -> String
capitalised [] = []
capitalised (head:tail) = Char.toUpper head : map Char.toLower tail

title :: [String] -> [String]
title [] = []
title (x:y) =  capitalised x : title y

