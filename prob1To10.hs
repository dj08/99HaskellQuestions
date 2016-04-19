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
