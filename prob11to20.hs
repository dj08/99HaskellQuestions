{-
Problem 11 Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has
no duplicates it is simply copied into the result list. Only elements
with duplicates are transferred as (N E) lists.

Example in Haskell:
P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data ElemWithCount a = Multiple Int a | Single a
     deriving (Show)     		     	      	      	       

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs@(y:ys) = (takeWhile (== y) xs) : pack (dropWhile (== y) xs)

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map f $ pack xs
            where f y  = (length y, head y)

encodeModified :: Eq a => [a] -> [ElemWithCount a]
encodeModified xs = map f (pack xs)
	       where f y | length y == 1 = Single (head y)
	       	       	 | otherwise     = Multiple (length y) (head y)
{-
Problem 12 Decode a run-length encoded list.

Given a run-length code list generated as specified in problem
11. Construct its uncompressed version.

Example in Haskell:
P12> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
-}

decodeModified :: [ElemWithCount a] -> [a]
decodeModified xs = concat (map g xs)
                    where
                      g (Single x) = [x]
                      g (Multiple n x) = take n $ repeat x

{-
Problem 13 Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method
directly. i.e. don't explicitly create the sublists containing the
duplicates, as in problem 9, but only count them. As in problem P11,
simplify the result list by replacing the singleton lists (1 X) by X.

Example in Haskell:
P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

encode' :: Eq a => [a] -> [(Int, a)]
encode' xs = foldr f [] xs
             where
               f x []                       = [(1, x)]
               f x (y@(a,b):ys) | x == b    = (1+a, x) : ys
                                | otherwise = (1, x) : y : ys

encodeDirect :: Eq a => [a] -> [ElemWithCount a]
encodeDirect xs = map g (encode' xs)
                  where g (1, x) = Single x
                        g (n, x) = Multiple n x

{-
Problem 14 Duplicate the elements of a list.

Example in Haskell:
> dupli [1, 2, 3]
[1,1,2,2,3,3]
-}

dupli :: [a] -> [a]
dupli xs = concatMap f xs
           where f x = [x, x]

{-
Problem 15 Replicate the elements of a list a given number of times.

Example in Haskell:
> repli "abc" 3
"aaabbbccc"
-}


repli :: [a] -> Int -> [a]
repli xs n = concatMap f xs
             where
               f x = take n $ repeat x

{-

