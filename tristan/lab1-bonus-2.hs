-- This took ~30 minutes

import Data.List

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

sumUnderLargestPrime :: Integer
sumUnderLargestPrime = sum (filter prime [2000000,1999999..0])

main = print (sumUnderLargestPrime)
