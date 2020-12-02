mult :: [Int] -> Int
mult = foldr (*) 1

posList :: [Int] -> [Int]
posList xs = filter pos xs 
    where
    pos x = x > 0

trueList :: [Bool] -> [Bool]
trueList xs = filter (==True) xs 

evenList :: [Int] -> [Int]
evenList xs = filter even xs 
    where
    even x = mod x 2 == 0

maxNumOfList :: (Ord a) => [a] -> a
maxNumOfList [] = error "empty list"
maxNumOfList [x] = x
maxNumOfList (x:xs) = max x (maxNumOfList xs)
maxList :: [[Int]] -> [Int]
maxList xs = map maxNumOfList xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = filter r xs
    where r x = a <= x && x <= b

posList :: [Int] -> [Int]
posList xs = filter pos xs 
    where
    pos x = x > 0
lenList :: [Int] -> Int
lenList []     = 0
lenList (x:xs) = 1 + lenList xs
countPositives ys = lenList $ posList ys

myLength :: [Int] -> Int
myLength xs = foldr (+) 0 (map o xs)
    where o x = 1

map' :: (a -> b) -> [a] -> [b]
map' f xs       = foldr (\x acc -> (f x):acc) [] xs
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs    = foldr (\x acc -> if p x then x:acc else acc) [] xs

myLength' :: [Int] -> Int
myLength' xs = foldr (\x acc -> 1 + acc) 0 xs