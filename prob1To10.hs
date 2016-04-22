{-
Problem 1 Find the last element of a list.

Example in Haskell:
Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}

-- Native implementation, without using any builtins
myLast :: [a] -> a
myLast [] = error "Empty List!"
myLast [x] = x
myLast (x:xs) = myLast xs

{-
Problem 2 Find the last but one element of a list.

Example in Haskell:
Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
-}

-- Native implementation, without using builtins
myButLast :: [a] -> a
myButLast [] = error "Empty List!"
myButLast [_] = error "List Too Small!"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

-- Or... much simpler way:
myButLast' :: [a] -> a
myButLast' [] = error "Empty List!"
myButLast' [_] = error "List Too Small!"
myButLast' xs = xs !! (length (xs) - 2)

-- Alternative, old code I wrote while learning hs
internetLastButOne :: [a] -> a
internetLastButOne xs = if length xs == 2
                        then head xs
                        else internetLastButOne (tail xs)

{-
Problem 3 Find the K'th element of a list. The first element in the
list is indexed 1.

Example in Haskell:
Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
-}

elementAt :: [a] -> Int -> a
elementAt _ 0 = error "First position is indexed 1!"
elementAt [] _ = error "List too small!"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

-- More concise using the infix operator
elementAt' :: [a] -> Int -> a
elementAt' _ 0 = error "First position is indexed 1!"
elementAt' xs n = xs !! (n-1)

{-
Problem 4 Find the number of elements of a list.

Example in Haskell:
Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
-}

myLength :: [a] -> Int
-- Approach: Recursively behead the list and keep a count
myLength (x:xs) = 1 + myLength xs
myLength []     = 0

-- Primitive recursion not good. Lets use folds
myLengthF xs = foldr step 0 xs
               where step _ x = x + 1

-- Another good one from haskell community
myLength' :: [a] -> Int
myLength' = sum . map ( \_ -> 1 )

{-
Problem 5 Reverse a list.

Example in Haskell:
Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1]
-}

myReverse = foldl step []
            where step x y = y:x

{-
Problem 6 Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:
*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True
-}

isPalindrome xs = xs == (reverse xs)

{-
Problem 7 Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example in Haskell:

-- A new data type, because lists in Haskell are homogeneous.
data NestedList a = Elem a | List [NestedList a]

*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]
-}

data NestedList a = Elem a | List [NestedList a]
                             deriving( Show ) -- Not really needed

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
-- Need (++) above instead of (:) because flatten x could return a list instead
-- More haskellish way: flatten x has type [a], not 'a'. Hence (++), not (:)

-- Now using folds, since recursion is primtive...
flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x] 		-- Base case
flatten' (List xs) = foldr (++) [] $ map flatten' xs

{-
Problem 8 Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example in Haskell:
> compress "aaaabccaadeeee"
"abcade"
-}

compress :: Eq a => [a] -> [a]
compress (x:ys@(y:_))
  | x /= y	= x : compress ys
  | otherwise	= compress ys
compress x = x -- Type declaration takes care of the rest

-- Using folds... not very convincing somehow...
compress' :: Eq a => [a] -> [a]
compress' xs = foldr step [] xs
  where step x [] = [x]
        step x y | x == head y	= y
                 | otherwise	= x : y

{-
Problem 9 Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

Example in Haskell:
*Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
-}

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs@(y:ys) = (takeWhile (== y) xs) : pack (dropWhile (== y) xs)

{-

Problem 10 Run-length encoding of a list. Use the result of problem
P09 to implement the so-called run-length encoding data compression
method. Consecutive duplicates of elements are encoded as lists
(N E) where N is the number of duplicates of the element E.

Example in Haskell:
Prelude> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map f $ pack xs
            where f y  = (length y, head y)
