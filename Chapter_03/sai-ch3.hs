--Sergey Ivanov - Haskell Club Chapter 3 3/7/2016
--
--
 {- problem 1
 - what is the largest prime factor of the number 600851475143 ?
 -}

-- ugh I have to test for primes?

ans_1 =  take 1 [y | y<- [floor (sqrt 600851475143), floor (pred (sqrt 60085147514))..], 60085147514 `mod` y == 0]
 {- problem 2
 - find the product of the pythagorean triple (i.e. a^2 + b^2 == c^2, a < b < c) such that a + b + c = 1000
 -}

ans_2 =  product ( head [[a,b,c] | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 1000] )


{- problem 3
 - flatten a nested list structure
 - in:  [1,2,3,4,[5,6,[7,8,9],10],11,12,[13,14,15,16],17]
 - out: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]
 -}

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem l)  = [l]
flatten (List l)  = flatten (head l) ++ flatten (List (tail l))
flatten (List []) = []


{- problem 4
 -  Make a thing that encodes stuff like this
 -  in: "aaaabccaadeeee"
 -  out: [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
 -}

encode [] = []    --this pattern matching function thing is wilddddddd
encode l  = [(length (takeWhile (== head l) l), head l)] ++ encode (dropWhile (== head l) l)
