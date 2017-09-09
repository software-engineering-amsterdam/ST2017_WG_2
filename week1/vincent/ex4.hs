-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~40 min

import Data.List

-- Helper functions

reverseNum :: Int -> Int
reverseNum = read . reverse . show

primes :: [Int]
primes = 2 : filter isPrime [3..] 

isPrime :: Int -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes


assignment4Set = [0..10000]
reversiblePrimes :: [Int]
reversiblePrimes = filter p assignment4Set
    where p x = isPrime (x) && isPrime (reverseNum (x))

main = print reversiblePrimes