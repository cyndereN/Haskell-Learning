mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z
{-Mult takes an integer x and returns a function,
which in turn takes an integer y and returns a
function, which finally takes an integer z and
returns the result x*y*z.-}

--This idea is made precise in the type for length by the inclusion of a type variable
length :: [a] -> Int
{-For any type a, length takes a list of values of
type a and returns an integer   -}
{-A type with variables is called polymorphic.-}

--This idea is made precise in the type for + by the inclusion of a class constraint
(+) :: Num a => a → a → a
{-For any type a in the class Num of
numeric types, + takes two values of
type a and returns another.-}
{-A type with constraints is called overloaded.-}

--A class is a collection of types that support certain operations, called the methods of the class.
{-
Eq - Equality types
Ord - Ordered types
Show - Showable types
Haskell has a number of basic classes, including:
Read - Readable types
Num - Numeric types
-}
{-
(==) :: Eq a ⇒ a → a → Bool
(<) :: Ord a ⇒ a → a → Bool
show :: Show a ⇒ a → String
read :: Read a ⇒ String → a
(∗) :: Num a ⇒ a → a → a
-}

abs :: Int -> Int
abs n | n >= 0 = n
      | otherwise = -n

(&&):: Bool -> Bool -> Bool
True && True = True
True && False = False
False && True = False
False && False = False
-- same as
True && True = True
_ && _ = False
--same as
False && _ = False
True && b = b

add x y = x+y
--same as
add = \x -> (\y -> x+y)

compose f g x = f (g x)
--same as
compose f g = \x -> f (g x)

safeTail : [a] -> [a]
safeTail xs = if null xs then [ ] else tail xs  --condition
--same as
safeTail xs | null xs = [ ]
            | otherwise = tail xs   --guarded
--same as
safeTail [ ] = [ ]
safeTail (x : xs) = xs     --pattern matching