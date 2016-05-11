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

Problem 32 Determine the greatest common divisor of two positive
integer numbers. Use Euclid's algorithm.

Example in Haskell:
[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
[9,3,3]-}

myGCD 0 x = abs(x)
myGCD x 0 = abs(x)
myGCD m n | m>n = myGCD (abs(m-n)) (abs n)
          | otherwise = myGCD (abs m) (abs(n-m))

-- Using mod is much better than using recursion. Saves iterations for
-- large numbers.
myGCD' a b | b == 0 = abs a
           | otherwise = myGCD b (mod a b)

{-
Problem 33 Determine whether two positive integer numbers are
coprime. Two numbers are coprime if their greatest common divisor
equals 1.

Example in Haskell:
* coprime 35 64
True-}

coprime m n = gcd m n == 1
