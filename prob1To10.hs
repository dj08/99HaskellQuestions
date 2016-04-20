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
