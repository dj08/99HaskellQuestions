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