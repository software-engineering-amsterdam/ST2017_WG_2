-- Question: Do you have to test that your answer is correct? How could this be checked?
-- Answer: we should always test if our answer is correct. We could check this by first validate all
-- submethods used by the implementation. We can assume that the haskell standard library is correct.
-- When we know that our prime function are correct for some N, we can assume that the combination of these
-- correct functions is correct.
--
-- This took ~30 minutes

import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

nextPrime :: Integer -> Integer
nextPrime n
    | prime n = n
    | otherwise = nextPrime (n + 1)

isSumPrime :: [Integer] -> Bool
isSumPrime p = prime (foldl (+) 0 p)

summedPrimes :: [Integer] -> [Integer]
summedPrimes p
    | p == [] = summedPrimes (take 101 primes)
    | isSumPrime p = p
    | otherwise = summedPrimes (tail p ++ [nextPrime ((last p) + 1)])

main = do
    let match = summedPrimes []
    let summed = foldl (+) 0 match
    print (match)
    print (length match)
    print (summed)
    print (prime summed)