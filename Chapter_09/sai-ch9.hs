import System.Random

-- warmup 1 : problem 23
-- Prompt : "Extract a given number of randomly selected elements from a list"
-- e.g. : rnd_select 3 "abcdefgh" 
--        -> eda
--helper

extract_elem :: Int -> [a] -> (a, [a])
extract_elem n l = (l !! n , take n l ++ drop (n+1) l)

rnd_select :: Int -> [a] -> [a]
rnd_select n [] = []
rnd_select 0 l = []
rnd_select n l = [fst l_x] ++ rnd_select (n-1) (snd l_x)
    where l_x = extract_elem  (fst ( randomR (0,length(l)-1) (mkStdGen n))) l

-- warmup 2 : problem 24
-- Prompt : "Lotto : Draw N different random numbers from the set 1..M"
-- e.g. diff_select 6 49
-- => [23, 1, 17, 33, 21, 37]

diff_select :: Int -> Int -> [Int]
diff_select a b = rnd_select 6 [1..b]


{-real stuff:

(a)
Make a program that counts the number of characters, words, and lines in a
file e.g.
```
haskell_wc poem.txt
420 69 7
```

returns the first number is characters, second is words, third is lines
(b)
Building on the previous example add options that let you specify to *only*
count that metric. Do this for chars, words and lines.

e.g.

```
haskell_wc -l poem.txt
7
```

```
haskell_wc -w poem.txt
69
```
-}



{-
(c)
Make a program that takes a sequence of numbers as an argument and inserts
them into a binary tree, then prints the elements of the tree inorder. Define
your own tree type for this.

(d)
Extend the previous program to let the user specify to either print inorder or
postorder

(e)
Add an option that uses a stack instead of a tree(this should be fairly
easy to print)
-}


