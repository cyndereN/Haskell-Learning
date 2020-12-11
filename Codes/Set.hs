data Set a = Nil | Node (Set a) a (Set a)
                  deriving (Show, Read, Ord)

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Ord a => Set a -> [a]
toList Nil = []
toList (Node l v r) = (toList l) ++ [v] ++ (toList r)

-- fromList: do not forget to remove duplicates!
fromList :: Ord a => [a] -> Set a
fromList [] = Nil
fromList xs = pBST Nil (quickSort xs)

--Normal Binary Search Tree
{-
fromList [] = Nil
fromList (x:xs) = insert x s 
                        where s = fromList xs
-}

--Perfect Binary Search Tree
pBST :: Ord a => Set a -> [a] -> Set a
pBST s [] = s
pBST s1 s2 = pBST (insert e s1) t 
                                where
                                  e = middleElement s2
                                  t = delete' e s2

delete' :: (Eq a) => a -> [a] -> [a]
delete' x [] = []
delete' y (x:xs) = if x == y then xs else x : delete' y xs

middleElement :: (Ord a) => [a] -> a
middleElement s = mE s s 

mE :: (Ord a) => [a] -> [a] -> a
mE    []    (h:s2) = h
mE (_:[])   (h:s2) = h
mE (_:_:s1) (_:s2) = mE s1 s2

quickSort :: (Ord a) => [a] -> [a] 
quickSort [] = []
quickSort (x:xs) = quickSort [a|a<-xs,a<=x] ++ [x] ++ quickSort [a|a<-xs,a>x]


-- test if two sets have the same elements (pointwise equivalent).
instance (Ord a) => Eq (Set a) where
  s1 == s2 = (toList s1 == (toList s2))

-- you should be able to satisfy this property quite easily


-- the empty set
empty :: Set a
empty = Nil

-- is it the empty set?
isNull :: Set a -> Bool
isNull Nil = True
isNull _ = False

-- build a one element Set
singleton :: a -> Set a
singleton x = Node Nil x Nil

-- insert an element *x* of type *a* into Set *s*. Make sure there are no
-- duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x Nil = Node Nil x Nil
insert x (Node l v r) = 
                      if x < v then 
                        Node (insert x l) v r
                      else if x > v then
                        Node l v (insert x r) 
                      else 
                        Node l v r

-- join two Sets together be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union Nil Nil = Nil
union Nil s1 = s1
union s2 Nil = s2
union s1 s2 = pBST Nil (quickSort (toList s1 ++ toList s2))

-- return, as a Set, the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection Nil _ = Nil
intersection _ Nil = Nil
intersection s1 s2 = pBST Nil (intersect' (toList s1) (toList s2))

intersect' :: (Ord a) => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' (x:xs) (y:ys)
    | x == y = x : intersect' xs ys
    | x < y = intersect' xs (y:ys)
    | otherwise = intersect' (x:xs) ys

-- all the elements in *s1* not in *s2*
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference Nil _ = Nil
difference s Nil = s
difference s1 s2 = pBST Nil (difference' (toList s1) (toList s2))

difference' :: (Ord a) => [a] -> [a] -> [a]
difference' [] _ = []
difference' (x:xs) (y:ys)
    | x == y = difference' xs ys
    | x > y = difference' (x:xs) ys
    | otherwise = x : difference' xs (y:ys)

-- is element *x* in the Set s1?
member :: (Ord a) => a -> Set a -> Bool
member _ Nil = False
member x (Node l v r) = 
                    if x == v then True
                    else if x < v then member x l
                    else member x r

-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality Nil = 0
cardinality (Node l _ r) = cardinality l + cardinality r + 1

-- apply a function to every element in the Set
setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f Nil = Nil
setmap f (Node l v r) = Node (setmap f l) (f v) (setmap f r)

-- right fold a Set using a function *f*

setfoldr :: (a -> b -> b) -> b -> Set a -> b
setfoldr _ b Nil = b
setfoldr f b (Node l v r) = setfoldr f b' l
   where
   b'  = f v b''
   b'' = setfoldr f b r

-- remove an element *x* from the set
-- return the set unaltered if *x* is not present
removeSet :: (Ord a) => a -> Set a -> Set a
removeSet a Nil = Nil
removeSet x (Node l v r) = 
  if x == v then
    union l r
  else if x < v then
    Node (removeSet x l) v r
  else if x > v then
    Node l v (removeSet x r)
  else
    Node l v r

-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Ord a => Set a -> Set (Set a)
powerSet t = pBST Nil (powersetTree t)

powersetTree :: (Ord a) => Set a -> [Set a]
powersetTree Nil = [Nil]
powersetTree t = 
  [insert v subtree | subtree <- pxs] ++ pxs where
    (Node l v r) = t
    pxs = powersetTree (removeSet v t)
