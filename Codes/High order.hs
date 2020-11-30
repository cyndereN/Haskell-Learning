twice :: (a -> a) -> a -> a
twice f x = f (f x)

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]
--or
map f [] = []
map f (x:xs) = f x : map f xs
    map (+1) [1,3,5,7]
=>[2,4,6,8]

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]
--or
filter p [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs
    filter even [1..10]
=>[2,4,6,8,10]

sum [] = 0
sum (x:xs) = x + sum xs
product [] = 1
product (x:xs) = x * product xs
and [] = True
and (x:xs) = x && and xs
--or
sum = foldr (+) 0
product = foldr (*) 1
and = foldr (&&) True

f :: [Int] -> Int
f xs = foldr (+) 0 (map sqr (filter pos xs))
where
sqr x = x*x
pos x = x > 0
--or
f :: [Int] -> Int
f xs = sum [x*x | x<-xs, x>0]

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

foldl :: (a -> b -> a) -> a -> [b] -> a
    foldl (\acc x -> acc + x) 0 [1,2,3,4]
=>10

(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x) 
f' :: Integer -> Integer
f' x = x + 1
g' :: Integer -> Integer
g' x = x * 3
h = f' . g'
    h 2
=>7
