data Answer = Yes | No | Unknown
answers :: [Answer]
answers = [Yes,No,Unknown]
flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

data Shape = Circle Float
            | Rect Float Float
square :: Float -> Shape
square n = Rect n n
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

data Maybe a = Nothing | Just a
return :: a -> Maybe a
return x = Just x
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>= _ = Nothing
Just x >>= f = f x

data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat (n+1) = Succ (int2nat n)

data Expr = Val Int
    | Add Expr Expr
    | Mul Expr Expr
Add (Val 1) (Mul (Val 2) (Val 3))    
size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--efficiency
reverse :: [a] -> [a]
reverse xs  = revInto [] xs
    where revInto ys [] = ys
          revInto ys (x:xs) = revInto (x:ys) xs

--Queue
data Q a = Q [a] deriving (Eq, Show)
empty :: Q a  -- an empty queue
add :: a -> Q a -> Q a -- add element at back
remove :: Q a -> Q a-- remove element from front
front :: Q a -> a -- inspect front element
isEmpty :: Q a -> Bool -- check if queue is empty
empty = Q []
add x (Q xs) = Q (xs ++ [x])
remove (Q (x : xs)) = Q xs
front (Q (x : xs)) = x
isEmpty (Q xs) = null xs

--smart queue
data Q a = Q [a] deriving (Eq, Show)
empty :: Q a  -- an empty queue
add :: a -> Q a -> Q a -- add element at back
remove :: Q a -> Q a-- remove element from front
front :: Q a -> a -- inspect front element
isEmpty :: Q a -> Bool -- check if queue is empty
empty = Q []
add x (Q xs) = Q (xs ++ [x])
remove (Q (x : xs)) = Q xs
front (Q (x : xs)) = x
isEmpty (Q xs) = null xs

data Credential = Mobile String
                | Email String
                | UserName String
validate :: Credential -> Bool
validate (Mobile num) = (length num) == 11
validate (Email address) = elem '@' address 
validate (UserName name) = 
  let 
    nameLength = length name
  in
    nameLength > 5 && nameLength <= 10

findUser :: Email -> Maybe User

data MyTree e = Leaf e
            | Node e (Tree e) (Tree e)
            deriving (Show)

leftMost :: MyTree a -> a
leftMost (Node _ left _) = leftMost left
leftMost (Leaf v) = v

myMap :: (a -> b) -> MyTree a -> MyTree b
myMap f (Leaf x) = Leaf (f x)
myMap f (Node x l r) = Node (f x) (myMap f l) (myMap f r)

data Option a = None
              | Some a
              deriving (Show)
 
myMap' :: (a -> b) -> Option a -> Option b
myMap' f None = None
myMap' f (Some x) = Some (f x)