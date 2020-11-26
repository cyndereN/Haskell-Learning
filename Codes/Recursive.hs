factorial :: Int -> Int 
factorial 0 = 1 
factorial n = n * factorial (n-1)

maxNumOfList :: (Ord a)=> [a] -> a
maxNumOfList [] = error "empty list"
maxNumOfList [x] = x
maxNumOfList (x:xs) = max x (maxNumOfList xs)

product' :: [Int] -> Int 
product' [] = 1 
product' (x:xs) = x * product' xs

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] 

repeat' :: a -> [a]
repeat' x = x: repeat' x

zip' :: [a] -> [a] -> [(a,a)]
zip' [] [] = []
zip' x [] = []
zip' [] y = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool 
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) 
        |  x < y = x:y:ys
        | otherwise = y : insert x ys
insertSort :: Ord a => [a] -> [a]
insertSort  [] = []
insertSort  (a:xs) = insert a (insertSort xs)

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

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort [a|a<-xs,a<=x] ++ [x] ++ quickSort [a|a<-xs,a>x]

selectFromOri :: Int -> [Int] -> [Int]
selectFromOri _ [] = []
selectFromOri x (y:ys)
           | x == y =ys 
           | otherwise = y:selectFromOri x ys

selectSort :: [Int] -> [Int]
selectSort [] = []
selectSort xs = mini : selectSort xs'
        where
            mini = minimum xs
            xs' = selectFromOri mini xs

swaps :: [Int] -> [Int]
swaps [] = []
swaps [x] = [x]
swaps (x1:x2:xs)
       | x1 > x2 = x2 : swaps(x1:xs)
       | otherwise = x1 : swaps(x2:xs)

bubbleSort :: [Int] -> [Int]
bubbleSort xs
           | swaps xs == xs =xs
           | otherwise = bubbleSort $ swaps xs