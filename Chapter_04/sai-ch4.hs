--Sergey Ivanov haskell club chapter 4 problems
--
--problem 1

-- sum of digits of number 2^1000
--this is a helper function for  sumDigits
sumString [] = 0; sumString x = fromEnum (head x) - fromEnum '0' + (sumString (tail x))

sumDigits x = sumString (show x)

let ans1 = sumDigits (2^1000)

--bonus
digitalRoot x = if (x<=9) then x else digitalRoot (sumDigits x)

--problem 2:
--Decode a run-length encoded list
expandPair p = replicate (fst p) (snd p)
decompress []= []
decompress l = expandPair (head l) ++ decompress (tail l)

--problem 3
--Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X. 
--e.g. * (encode-direct '(a a a a b c c a a d e e e e))
--       result : ((4 A) B (2 C) (2 A) D (4 E))
