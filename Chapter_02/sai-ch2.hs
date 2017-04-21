--Sergey Ivanov - Haskell Club Chapter 2 3/31/2017
--
 {- problem 1 
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
 -}

ans1 = sum [ a | a <- [1..100], a `mod` 3 == 0 || a `mod` 5 == 0]


--problem 2, sum of fibonacci until they become > 4000000
--
-- Method 1 - raw
basicfib n = if n <= 1
                then n
                else basicfib (n-1) + basicfib (n-2)

--it seems some functions can be vectorized to accept list based on types. We are not there yet, so I used the "map" function to do the same thing here
ans2_raw = sum (takeWhile (<4000000) (map basicfib [1..]))


--takeWhile --use this to take from an list while a condition is true


-- Method 2
--this is the canonical fibonacci function, we should use this to sum via list power
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
ans2 = sum [x | x <- takeWhile (<4000000) fibs]


--problem 3 reverse a list


--method 1 : with init and last
--probably the easiest way to do it
myReverse x = if x /= []
                 then [last x] ++ myReverse (init x)
                 else []

--method 2 : with head and tail
myReverse' x = if x /= []
                  then myReverse' (tail x)  ++ [head x]
                  else []



--problem 4
--I guess we're doing the "test palindrome" problem instead here

isPalindrome x = if length x <= 1
                    then True
                    else (head x == last x) && isPalindrome(init (tail x))
