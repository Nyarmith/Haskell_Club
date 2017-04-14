replicate' :: [a] -> Int -> [a]
replicate' [] _     = []
replicate' xs 0     = []
replicate' xs 1     = xs
replicate' (x:xs) n = (take n (repeat x)) ++ (replicate' xs n) 

{- Use takeWhile and dropWhile to split the list between same elements. -}
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack l = [(takeWhile (== head l) l)] ++ (pack (dropWhile (== head l) l))

{- Same as above, except the lhs of ++ is a tuple of (length, elem) -}
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode l  = [(length (takeWhile (== head l ) l), head l)] ++ encode (dropWhile (== head l) l)

data ListItem a = Single a | Multiple Int a
    deriving (Show)

{- Use a helper function to pattern match our way to the appropriate
   sub-type. -}
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified [] = []
encodeModified l = [encodeHelper x | x <- (encode l)]
    where encodeHelper :: (Eq a) => (Int, a) -> ListItem a 
          encodeHelper (1, x) = Single x
          encodeHelper (n, x) = Multiple n x

{- Use a helper function to pattern match out the sub-type.  Since this
   helper function must return a list for `Multiples', use `concat' to
   flatten the resulting list of lists. -}
decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified [] = []
decodeModified l  = concat [decodeHelper x | x <- l]
    where decodeHelper :: (Eq a) => ListItem a -> [a]
          decodeHelper (Single x) = [x]
          decodeHelper (Multiple n x) = replicate n x
