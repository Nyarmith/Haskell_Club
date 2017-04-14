rotate :: [a] -> Integer -> [a]
rotate []     _ = []
rotate xs     0 = xs
rotate (x:xs) n = rotate (xs ++ [x]) (n-1)
