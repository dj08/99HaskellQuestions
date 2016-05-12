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

{-
Problem 34 Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the number of
positive integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case:
phi(1) = 1.

Example in Haskell:
* totient 10
4-}

totient m = length $ filter (coprime m) [1..m-1]

{-
*Main> map totient [1..20]
[0,1,2,2,4,2,6,4,6,4,10,4,12,6,8,8,16,6,18,8]

Note that prime numbers constantly come up as the local maxima of the
totient function... obviously! In fact, prime numbers will NECESSARILY
have totient m = m-1. -}

{-
Problem 35 Determine the prime factors of a given positive
integer. Construct a flat list containing the prime factors in
ascending order.

Example in Haskell:
> primeFactors 315
[3, 3, 5, 7]-}

-- There is going to be only 1 such list possible. We don't need to
-- check for prime factors if we are checking in ascending order!

-- A no brainer first
primeFactors :: Integer -> [Integer]
primeFactors m
  | isPrime m = [m]
  | otherwise = factor : primeFactors (m `div` factor)
  where
    limit = truncate (sqrt (fromIntegral m))
    factors = filter (isPrime) [ x | x <- [2..limit], rem m x == 0 ]
    factor = head factors

-- Bit more intelligent - identifying prime divisors by ascesion.
primeFactors' :: Integer -> [Integer]
primeFactors' m = foldr f [] (take (fromInteger limit) (repeat [2..limit]))
   where
     limit = truncate (sqrt (fromInteger m))
     f x acc | quot' `rem` res == 0 = res:acc
             | otherwise            = acc
                 where
                   res = head (filter (\y -> rem m y == 0) x)
                   quot' = foldr g m acc
                          where g n acc2 = acc2 `div` n

{-
This doesn't work, because the list is traversed only once...
primeFactors' m = foldr f [] [2..limit]
   where limit = truncate (sqrt (fromInteger m))
         f x acc | quot `rem` x == 0 = x:acc
                 | otherwise         = acc
                 where quot = foldr g m acc
                              where g n acc2 = acc2 `div` n
-}
