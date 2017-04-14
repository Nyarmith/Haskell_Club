replicate' :: [a] -> Int -> [a]
replicate' [] _     = []
replicate' xs 0     = []
replicate' xs 1     = xs
replicate' (x:xs) n = (take n (repeat x)) ++ (replicate' xs n) 
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack l = [(takeWhile (== head l) l)] ++ (pack (dropWhile (== head l) l))

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode l  = [(length (takeWhile (== head l ) l), head l)] ++ encode (dropWhile (== head l) l)

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified [] = []
encodeModified l = [encodeHelper x | x <- (encode l)]
    where encodeHelper :: (Eq a) => (Int, a) -> ListItem a 
          encodeHelper (1, x) = Single x
          encodeHelper (n, x) = Multiple n x

-- the `x' in `decodeHelper x' is apparently not the expected type,
-- but I don't really see how this is the case
{-decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified [] = []
decodeModified l  = [decodeHelper x | x <- l]
    where decodeHelper :: (Eq a) => ListItem a -> [a]
          decodeHelper (Single x) = [x]
          decodeHelper (Multiple n x) = replicate n x-}
