data Tree a = Leaf a | Branch (Tree a)(Tree a)
                      deriving(Eq, Ord, Show, Read)

treeMap :: (a -> b) -> Tree a -> Tree b 
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch l r) = Branch (treeMap f l)(treeMap f r)

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b 
treeFold fb fl = g where g (Leaf x) = fl x
                         g (Branch l r) = fb (g l)(g r)

t1 :: Tree Int
t1 = Branch (Branch (Leaf 1)(Leaf 1))(Leaf 2)