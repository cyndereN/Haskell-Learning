import qualified Data.Char 
capitalized :: String -> String
capitalized [] = []
capitalized (head:tail) = Data.Char.toUpper head : lowered tail
  where
    lowered [] = []
    lowered (head:tail) = Data.Char.toLower head : lowered tail

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) 
        |  x < y = x:y:ys
        | otherwise = y : insert x ys
iSort :: Ord a => [a] -> [a]
iSort  [] = []
iSort  (a:xs) = insert a (iSort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
        | x > y = y:merge (x:xs) ys
        | otherwise = x:merge xs (y:ys)
        
mergeSort :: Ord a => [a] -> [a]
mergeSort xs = merge (mergeSort x1) (mergeSort x2)
        where
            (x1, x2) = split xs
            split xs = (take mid xs, drop mid xs)
            mid = (length xs) `div` 2

import qualified Data.Char 
lst = ['A'..'N']
dgt = 6

ccypher :: [Char] -> Int -> [Char]
ccypher (x:[]) n = [x]
ccypher (x:xs) n = (drop n (x:xs)) ++ (take n (x:xs))

makeKey :: Int -> [(Char, Char)]
makeKey n = zip (ccypher lst n) lst 

searchSnd :: Eq a => [(a, b)] -> a -> b
searchSnd = flip searchSnd'
    where searchSnd' key = snd . head . filter ((== key) . fst)

lookUp :: Int -> Char -> Char
lookUp n c = searchSnd (makeKey n) c

tupleSwap :: [(Char,Char)]->[(Char,Char)]
tupleSwap (x:xs) = (snd x,fst x) : (tupleSwap xs)

encipher :: Int -> Char -> Char
encipher n c = searchSnd (tupleSwap (makeKey n)) c


normalise :: String -> String
normalise [] = []
normalise (x:xs) 
    | Data.Char.isAlpha x = Data.Char.toUpper x : normalise xs
    | otherwise           = normalise xs

encipherStr :: Int -> String -> String
encipherStr n [] = []
encipherStr n (x:xs) 
    | Data.Char.isAlpha x = encipher n (Data.Char.toUpper x): encipherStr n (normalise xs)
    | otherwise = encipherStr n (normalise xs)