{-
Problem 31 Determine whether a given integer number is prime.

Example in Haskell:
P31> isPrime 7
True
-}

isPrime :: Integer -> Bool

-- Prime numbers only occur near 6k
probablePrimes = 2 : 3 : concatMap f (zip [5, 11 ..] [7, 13 ..])
                 where f (x, y) = [x, y]

-- Better like this
probablePrimes' = 2 : 3 : [ x+i | x <- [6, 12..], i <- [-1, 1] ]

isPrime x = foldr g True (takeWhile (<= (ceiling (sqrt (fromInteger x))))
                          probablePrimes)
  where
    g y acc = ( mod x y /= 0 ) && acc
    

{-
Problem 32 Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.

Example in Haskell:
[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
[9,3,3]
-}

myGCD :: Int -> Int -> Int
myGCD x 0 = abs x
myGCD a b = myGCD b (mod a b) 