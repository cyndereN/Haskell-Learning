biggestInt, smallestInt :: Int
biggestInt  = maxBound
smallestInt = minBound
    biggestInt
=> 9223372036854775807
    smallestInt
=> -9223372036854775808

swap::(Int,Int)->(Int,Int)
swap(x,y) = (y,x)
    swap(1,2)
=> (2,1)

sumtorial :: Integer -> Integer 
sumtorial 0 = 0 
sumtorial n = n + sumtorial (n-1)
    sumtorial 5
=> 15

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1
    hailstone 45
=> 136

f :: Int -> Int -> Int -> Int
f x y z = x + y + z
n=1
    f 3 (n+1) 7
=> 12

succ 5
=> 6
succ 'x'
=> 'y'
succ 60*3
=> 183
succ(60*3)
=> 181

hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']
hello2 :: String
hello2 = "hello"
helloSame = hello1 == hello2
    helloSame
=> True

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)
    hailstoneSeq 5
=> [5,16,8,4,2,1]

-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs
{-The first clause says that the length of an empty list is 0. 
The second clause says that if the input list looks like (x:xs), 
that is, a first element x consed onto a remaining list xs, 
then the length is one more than the length of xs.
Since we don’t use x at all we could also replace it by an underscore: 
intListLength (_:xs) = 1 + intListLength xs.-}
    intListLength [1,41,123]
=> 3

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs
    sumEveryTwo [1,1,2,2,3,3,5,6]
=> [2,4,6,11]

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
    hailstoneLen 5
=> 5
-----------------------------------------------------------
:l baby

    [1,2,3,4] ++ [7,8,9]     --prepending--
=> [1,2,3,4,7,8,9]
    “Hello” ++ “ “ ++ “World!”
=> “Hello World!”
    [‘H’,’e’,’l’] ++ [‘l’,’o’]
=> “Hello”

    ‘A’ : “ BIG MESS”       --appending--
=> “A BIG MESS”
    54 : [45,69,27]
=> [54,45,69,27]

    [3.4,7.89,9.4,12.0] !! 3  --use the !! operator to get a list member by index number
=> 12.0 

    head [9,7,3,7]
=> 9
    tail [9,7,3,7]
=> [7,3,7]
    last [9,7,3,7]
=> 7
    init [9,7,3,7]
=> [9,7,3]
    null [ ]
=> True
    take 3 [9,7,3,7]
=> [9,7,3]
    drop 3 [9,7,3,7]
=> [7]
    maximum [2,2,8,5,90,3]
=> 90
    sum [9,7,3,7]
=> 26
    product [9,7,3,7]
=> 1323
    4 `elem` [9,7,3,7]
=> False

    [1..10]
=> [1,2,3,4,5,6,7,8,9,10]
    [‘m’..’q’]
=> “mnopq”
    [2,4..10]
=> [2,4,6,8,10]
    [3,6..30]
=> [3,6,9,12,15,18,21,24,27,30]
    [20,19..1]
=> [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]

    take 11 [13,26..]
=> [13,26,39,52,65,78,91,104,117,130,143]
    take 5 (cycle [4,3,2])
=> [4,3,2,4,3]
    take 6 (repeat 7)
=> [7,7,7,7,7,7]
    replicate 5 ‘a’
=> “aaaaa”

    [x*2 | x <- [1..10]]
=> [2,4,6,8,10,12,14,16,18,20]
    [x*2 | x <- [1..10], x*2 >= 12]
=> [12,14,16,18,20]
    [x | x <- [50..100], x `mod` 7 == 5]
=> [54,61,68,75,82,89,96]
    [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
=> [55,80,100,110]

    [ [x | x <- xs, even x] | xs <- xxs ]
=> [[8,6,2],[2,4,6],[12,6,8,4,6]]


    fst (45, 78)
=> 45
    snd (45, 78)
=> 78

    zip [5,3,4,2,6,7,8,9,0,1,2,3] [5,6,7]
=> [(5,5),(3,6),(4,7)]

    rightTriangles'
=> [(8,6,10)]