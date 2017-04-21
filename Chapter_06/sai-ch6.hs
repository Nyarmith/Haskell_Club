 -- Sergey Ivanov - Haskell Club Chapter 6 4/20/2016 (it's the weed day(lol(weed)))
 --

 -- Some notes for review
 -- *Typeclasses are basically interfaces
 -- *Main typeclasses : Eq( for equality testing), Ord(for ordering things e.g. >), Show (for presenting something as a string), Read (for inputting a string), Enum(for sequentially ordered types that can be enumerated liek numbers), Bounded(for things that need an upper lower bound), Num(for things that are numeric)
 -- *you an separate function bodies for different cases
 -- *you should give everything that isn't a small function types
 -- *you can manually alert errors with 'error "string"'
 -- *a list in haskell looks like 1:2:3:[] so that's what you use for pattern matching (: means dereference the next elem in the linked list)
 -- *in addition to normal pattern matching, you can do "funcName @xs(y:ys) = <body>" and xs will reference the whole array with y:ys doing standard pattern matching
 -- *guards help expand subcases of some argument, and you can specify subcases of multi args with "where" clause
 -- *we call and define functions as infix with backticks(good for comparisons and things that should look like binary operators)


-- problem 1 Reverse a list == *advanced mode*
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) =  reverseList xs ++ [x]

-- problem 2 Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

-- this version was actually more clean than I was comfortable with and I thought it made it less clear
-- isPalindrome (x:xs) = x == last xs && (isPalindrome . init) xs

-- problem 3 Rotate a list N places to the left. (work for negative values)
-- maybe reverse it when the case is negatve?
-- consider using function guards as well here

-- Insert an element at a given position into a list.
-- maybe reverse it when the case is negatve?

