-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~30 min

import Data.List

-- Helper functions

primes :: [Int]
primes = 2 : filter isPrime [3..] 

isPrime :: Int -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes


getListPrimes :: [Int] -> [Int]
getListPrimes xs = if not $ isPrime (head (xs) + 1) then [head(xs)] else getListPrimes (tail xs)

assignment6 :: [Int]
assignment6 = getListPrimes primes

main = print assignment6