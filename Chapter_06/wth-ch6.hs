-- Question 5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = foldr (\x acc -> acc ++ [x]) [] xs

-- Question 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == reverse' xs

-- Question 19
rotate :: [a] -> Integer -> [a]
rotate [] _ = []
rotate xs  0 = xs
rotate (x:xs) n = rotate (xs ++ [x]) (n-1)

-- Question 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)
