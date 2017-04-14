--haskell chapter 5 problems
--
--
--run length encoding 
--
-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))


--

--problem 2
{-
(**) Replicate the elements of a list a given number of times.

Example:

* (repli '(a b c) 3)
(A A A B B B C C C)
-}
--
 --
repli [] n = []; repli l n = replicate n (head l) ++ repli (tail l) n
