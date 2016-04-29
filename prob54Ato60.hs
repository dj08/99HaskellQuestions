-- Initialization: setup data structures, etc.
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))

leaf x = Branch x Empty Empty

tree1' = Branch 'a' (Branch 'b' (leaf 'd')
                                (leaf 'e'))
                    (Branch 'c' Empty
                                (Branch 'f' (leaf 'g')
                                            Empty))

-- A binary tree consisting of a root node only
tree2 = Branch 'a' Empty Empty
 
-- An empty binary tree
tree3 = Empty
 
-- A tree of integers
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

-- Done with Initialization

-- Probelm 54 does not need a solution. We are already using type checks!

-- Placeholder for prob 55

{-
Problem 56 Symmetric binary trees

Let us call a binary tree symmetric if you can draw a vertical line
through the root node and then the right subtree is the mirror image
of the left subtree. Write a predicate symmetric/1 to check whether a
given binary tree is symmetric. Hint: Write a predicate mirror/2 first
to check whether one tree is the mirror image of another. We are only
interested in the structure, not in the contents of the nodes.

Example in Haskell:

*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
False
*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
True
-}

symmetric :: (Eq a) => Tree a -> Bool
symmetric Empty            = True
symmetric (Branch _ b1 b2) = isReflection b1 b2

-- An equality checker just ignoring the branch name is not going to
-- work.  We need a symmetry - hence a mirror reflection check.
isReflection :: Tree a -> Tree a -> Bool
isReflection b1 b2 = isEqIgnoringName b1 b2
  where isEqIgnoringName Empty Empty = True
        isEqIgnoringName Empty _     = False
        isEqIgnoringName _     Empty = False
        isEqIgnoringName (Branch _ t1 t2) (Branch _ t3 t4) =
          (isEqIgnoringName t2 t3) && (isEqIgnoringName t1 t4)
                     
-- Even simpler, courtesy haskell wiki solutions page:
symmetric' t = isReflection t t

