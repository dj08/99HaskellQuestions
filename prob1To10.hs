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
