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

and2, or2, nand2, nor2, xor2, impl2, equ2 :: Bool -> Bool -> Bool

-- First define equality, since we can use it in future solutions
equ2 True True = True
equ2 False False = True
equ2 _ _ = False

and2 a b = foldr (equ2) True [a, b, True]
or2 = undefined
xor2 = undefined
nand2 = undefined
nor2 = undefined
impl2 = undefined