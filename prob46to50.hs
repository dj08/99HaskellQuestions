{-

Problem 46 Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2
and equ/2 (for logical equivalence) which succeed or fail according to
the result of their respective operations; e.g. and(A,B) will succeed,
if and only if both A and B succeed.

A logical expression in two variables can then be written as in the
following example: and(or(A,B),nand(A,B)).

Now, write a predicate table/3 which prints the truth table of a given
logical expression in two variables.

Example in Haskell:
> table (\a b -> (and' a (or' a b)))
True True True
True False True
False True False
False False False
-}

and2, or2, nand2, nor2, xor2, impl2, equ2, nexor2 :: Bool -> Bool -> Bool

-- First define equality and negation, since we can use it in future solutions
and2 = (&&)
or2  = (||)
equ2 = (==)

nexor2 a b = foldr (equ2) True [a, b] -- More like, nequ2
xor2 a b = not $ nexor2 a b 

nand2 a b = not $ and2 a b
nor2 a b = not $ or2 a b
impl2 a b = (not a) `or2` b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = putStrLn $ concatMap (++ "\n" )
          [show a ++ " " ++ show b ++ " " ++ show (f a b)
          | a <- [True, False], b <- [True, False] ]

tableTuples f = [(a, b, (f a b)) | a <- [True, False], b <- [True, False] ]
