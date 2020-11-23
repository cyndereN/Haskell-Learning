doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNum x = if x > 100 then x else x * 2

doubleSmallNum x = (if x > 100 then x else x * 2) + 1

length’ xs = sum [ 1 | _ <- xs]

keepLowerCase st = [ c | c <- st, c ‘elem’ [‘a’..’z’]]

xxs = [[1,3,5,7,8,7,6,2],[2,3,4,5,6,1],[12,6,7,8,9,4,6,77]]

a = 1  --Same as let a = 1 in GHCi--

triples = [ (a,b,c) | c <- [1..10], a <- [1..10], b <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2 ]
rightTriangles' = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24 ]