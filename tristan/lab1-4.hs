-- Question: How would you test this function, by the way?
-- Answer: I would first make sure that my prime, primes and reversal are tested.
-- We could test the prime function by comparing a lot of N against our prime implementation and some known to be correct prime implementation
-- Then we should test the primes function. We should test this function against a list of known consecutive primes.
-- When these are tested, we apply reversal to all primes and run these test again. There needs to be some way to test reversal implementation first.
--
-- This took ~20 minutes

import Data.List

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

reversal :: Integer -> Integer
reversal = read . reverse . show

reversablePrime :: Integer -> Bool
reversablePrime n = prime n && prime (reversal n)

reversablePrimes :: [Integer]
reversablePrimes = filter (reversablePrime) primes

main = print (takeWhile (<10000) reversablePrimes)