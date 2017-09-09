-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~40 min

import Data.List

-- Helper functions

primes :: [Int]
primes = 2 : filter isPrime [3..] 

isPrime :: Int -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes


sum101primes :: [Int] -> Int
sum101primes xs = if isPrime $ sum $ take 101 xs then sum $ take 101 xs else sum101primes (tail xs)

main = print $ sum101primes primes