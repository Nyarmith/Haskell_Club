-- Sergey Ivanov : Haskell Club Chapter 8

import Data.List
-- Problem 31: prime test
-- literally sieve of eratosthenes
primesUntil m = sieve [2..m]
        where 
        sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
        sieve [] = []

isPrime n = n `elem` primesUntil n

-- Problem 32 : gcd
gcd' :: Int -> Int -> Int
gcd' 0 m = m
gcd' n 0 = n
gcd' n m = gcd' (m `mod` n) n

-- Problem 33 : test if two integers are coprime
coprime m n = (gcd' m n) == 1

--Problem 34 : Euler's Totient Function phi(m) = # of integers < m that are coprime to m
phi :: Int -> Int
phi m = sum $ map (\x -> fromEnum $ 1 == gcd' m x) [2..m-1]

--problem 35 : Determine the prime factors of a given positive integer
getFactors :: Int -> [Int]
getFactors 1 = []
getFactors n = filter (\x -> n `mod` x /= 0) (primesUntil n)


--problem 36 : Determine the prime factors of a given positive integer + their multiplicity
multiplicity :: Int -> Int -> (Int, Int)
multiplicity n x = (n, fromEnum (n `mod` x == 0) + multiplicity n/x x)
getListMultiplicity n (x:xs) = [multiplicity n x] ++ getListMultiplicity n xs

getFactors' :: Int -> [Int]
getFactors' 1 = []
getFactors' n = getListMultiplicity $ filter (\x -> n `mod` x != 0) (primesUntil n)

--problem 37 : Calculate Euler's totien phi(m) (improved)

totient :: Int -> Int
totient n = totientHack l where
        l = getFactors' n
        totientHack (x:xs) = (fst x - 1)*(fst x)^(snd x - 1) * totientHack xs
        totientHack [] = 1
