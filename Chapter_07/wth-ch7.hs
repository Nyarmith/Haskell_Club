{- 99 Questions, #13                                         -
 - encodeDirect: direct run length encoding without sublists -}
data ListItem a = Single a | Multiple Int a
    deriving (Show)

-- nothing to see here
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = helper 1 x xs

-- fold the list, counting the number of like elements
helper :: (Eq a) => Int -> a -> [a] -> [ListItem a]
helper n y [] = [listItemize n y]
helper n y (z:zs)
    | y == z = helper (n+1) y zs
    | otherwise = listItemize n y : helper 1 z zs

-- ListItem constructor
listItemize :: (Eq a) => Int -> a -> ListItem a
listItemize 1 y = Single y
listItemize n y = Multiple n y

{- 99 Questions, #14                           -
 - duplicate: duplicate every element in a list -}
duplicate :: [a] -> [a]
duplicate = foldr (\x xs -> x : x : xs) []

{- 99 Questions, #16                         -
 - drop: drop every n'th element from a list -}
dropEvery :: [a] -> Int -> [a]
dropEvery xs n
    | length xs < n = xs
    | otherwise = take (n-1) xs ++ dropEvery (drop n xs) n

{- 99 Questions, #17                      -
 - split: split the list at a given index -}
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs n = (take n xs, drop n xs)

{- 99 Questions, #18                                   -
 - slice: extract the sublist between elements m and n -}
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs m n
    | n < m     = []
    | otherwise = take (n-m+1) (drop (m-1) xs)

{- 99 Questions, #19                              -
 - removeAt: remove the nth element from the list -}
removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 xs = xs
removeAt n xs = take (n-1) xs ++ drop n xs

{- 99 Questions, #8                                    -
 - compress: eliminate adjacent duplicates from a list -}
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (dropWhile (== x) xs)

{- 99 Questions, #9                             -
 - pack: pack adjacent duplicates into sublists -}
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = [(takeWhile (== head xs) xs)] ++ (pack (dropWhile (== head xs) xs))
