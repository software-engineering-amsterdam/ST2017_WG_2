-- About 45 minutes
-- The first 20 Mersenne primes can be discovered relatively quickly.
-- Compared to the list from https://primes.utm.edu/mersenne/index.html#known,
-- they are all correct.

-- Using k=1, getting the first 20 Mersenne primes takes about 25 seconds,
-- while using k=6 (exercise 6 shows that from k=6 and higher the check is not fooled by carmichael numbers),
-- it takes about 30 seconds. Both ks return the correct first 20 Mersenne primes.

module Ex7 where

import Data.List
import System.Random
import Lecture6

-- List from https://primes.utm.edu/mersenne/index.html#known
mersennes :: [Integer]
mersennes = [2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521,
            607, 1279, 2203, 2281, 3217, 4253, 4423, 9689, 9941,
            11213, 19937, 21701, 23209, 44497, 86243, 110503, 132049,
            216091, 756839, 859433, 1257787, 1398269, 2976221, 3021377,
            6972593, 13466917, 20996011, 24036583, 25964951]

mersenneExponents :: [Integer]
mersenneExponents = [p | p <- primes, prime (2^p - 1)]

getKMersenne :: Integer -> Integer -> [Integer] -> IO [Integer]
getKMersenne k n z = if genericLength z == n then return z else do
    i <- primeMR 1 (2^p - 1)
    if i then getKMersenne (k+1) n (z++[p]) else getKMersenne (k+1) n z
    where p = primes !! (fromIntegral k)

-- Returns n mersenne exponents
mersenne :: Integer -> IO [Integer]
mersenne n = getKMersenne 0 n []

-- Find the first 20 mersenne exponents
main = do
    let n = 20 :: Int
    m <- mersenne (fromIntegral n)
    print m
    print (take n mersennes == m)