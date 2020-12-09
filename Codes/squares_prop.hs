import Test.QuickCheck

square :: Integer -> Integer
square x = x * x

pyth :: Integer -> Integer -> Integer
pyth a b = square a + square b

prop_square :: Integer -> Bool
prop_square x =
    square x >= 0

prop_squares :: Integer > Integer —> Bool
prop_squares x y =
    square (x+y) == square x + square y + 2*x*y

prop_pyth :: Integer -> Integer -> Bool
prop_pyth x y =
    square (x+y) == pyth x y + 2*x*y

cwordFind :: Char -> Int -> Int -> [String] -> [String]
cwordFind letter pos len words =
        [wd|wd <- words, length wd == len, wd !! pos == letter]

cwordFindRec :: Char -> Int -> Int -> [String] -> [String]
cwordFindRec letter pos len [ ] = [ ]
cwordFindRec letter pos len (x:xs) =
    if (x !! pos == letter) && (length x == len) then
        x:(cwordFindRec letter pos len xs) else

cwordFindRec letter pos len xs
cwordFindHO :: Char -> Int -> Int -> [String] -> [String]
cwordFindHO letter pos len words =
    filter p words
        where p x = (x !! pos == letter) && (length x == len)

prop_cwfCompRec :: Char -> Int -> Int -> Bool
prop_cwfCompRec letter pos len =
    cwordFind letter pos len == cwordFindRec letter pos len

prop_cwfRecHO :: Char -> Int -> Int -> Bool
prop_cwfRecHO letter pos len =
    cwordFindRec letter pos len == cwordFindHO letter pos len