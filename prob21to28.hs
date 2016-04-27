import System.Random -- needed prob 23 onwards
{-
Problem 21 Insert an element at a given position into a list.

Example in Haskell:
P21> insertAt 'X' "abcd" 2
"aXbcd"
-}

insertAt :: a -> [a] -> Int -> [a]
insertAt y xs n = [ x | (i, x) <- zip [1..n-1] xs ] ++
                  y : 
                  [ x | (i, x) <- zip [1..] xs, i > n-1 ]

-- Bit more efficient... we don't need list comprehension all the time!
insertAt' y xs n = take (n-1) xs ++ y : drop (n-1) xs

{-
Problem 22 Create a list containing all integers within a given range.

Example in Haskell:
Prelude> range 4 9
[4,5,6,7,8,9]
-}

range :: Int -> Int -> [Int]
range m n = [m..n]

{-
Problem 23
Extract a given number of randomly selected elements from a list.

Example in Haskell:
Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda
-}

rnd_select :: [a] -> Int -> [a] -- Do we have to return a string?
rnd_select xs n = (xs !! f) : rnd_select xs (n-1)
                  where f = getStdRandom (randomR (1, length xs))
