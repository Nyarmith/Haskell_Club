-- simple list comprehension
multiplesOf3And5 = sum [ x | x <- [1..1000], x `mod` 3 == 0, x `mod` 5 == 0 ]

-- fibonnaci helper function
fibonnaci n = if n == 1 || n == 2
                 then 1
                 else fibonnaci (n - 1) + fibonnaci (n - 2)
-- infinite list of fibonnaci numbers
fibs = [ fibonnaci i | i <- [1..] ]
-- hard code the number of terms, not sure what else to do
fourMillionFibSum = sum [ x | x <- takeWhile (<= 4000000) fibs, even x ]

-- construct list from individual elements taken in revese order
reverse' l = [ l !! ((length l) - i) | i <- [1..length l] ]

{- 
    - divide into three cases:
    - 1. empty list or list of length 1, base case
    - 2. list of length 2, base case
    - 3. list of length >= 3, recursive case
-}
isPalindrome l = if length l == 0 || length l == 1
                    then True
                    else if length l == 2
                            then l !! 0 == l !! 1
                            else (l !! 0 == l !! ((length l) - 1))
                                 && isPalindrome (init (tail l))
