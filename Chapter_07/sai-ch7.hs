--  Sergey Ivanov  --  Haskell Club Chapter 7  
-- 

-- problem 13
-- Run-length encoding of a list

mapGtOne n = if (n > 1) then "Multiple " ++ show n else show n

encodeDirect [] = []
encodeDirect (x:xs) = [mapGtOne ((length (takeWhile (== x) xs)) + 1) ++ [x]] ++ encodeDirect (dropWhile (== x) xs)

-- problem 14
-- Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x] ++ [x] ++ dupli xs

-- problem 16
-- Drop every nth element of a list
dropEvery :: Int -> [a] -> [a]
dropEvery [] _ = []
dropEvery n l = take (n-1) l ++ dropEvery (drop n l) n

-- problem 17
-- Split a list into two parts; the length of the first part is given.
split :: Int -> [a] -> ([a],[a])
split n l = (take n l, drop n l)

-- problem 18
-- Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice l a b = (take (b-a+1) . drop (a-1)) l


-- problem 20
-- Remov the Kth element of a list
removeAt :: Int -> [a] -> [a]
removeAt n l = take (n-1) l ++ drop n l

-- problem 8
-- compress
compress :: [a] -> [a]
compress []     = []
compress (x:xs) = [x] ++ encodeDirect (dropWhile (== x) xs)

-- problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists
encode :: [a] -> (Int, Char)
encode []     = []
encode (x:xs) = [((length (takeWhile (== x) xs)) + 1,x)] ++ encodeDirect (dropWhile (== x) xs)
