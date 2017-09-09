-- About 45min

-- Question: Do you have to test that your answer is correct? How could this be checked?
-- Answer: we should always test if our answer is correct. We could check this by first validate all
-- submethods used by the implementation. We can assume that the haskell standard library is correct.
-- When we know that our prime function are correct for some N, we can assume that the combination of these
-- correct functions is correct.
-- There is no known mathematical formula that immediately tells us what the answer is.
-- However, we can check by hand if the answer is correct by summing 101 prime numbers in a sliding window 
-- and seeing which smallest 101 primes are summing to a prime as well.

module Lab1 where
import Data.List
import Test.QuickCheck

-- Functions from the lab exercise page
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

exerciseFive :: [Integer] -> Integer
exerciseFive x
    | prime xs = xs
    | otherwise = exerciseFive (tail x)
    where xs = sum (take 101 x)

main = print (exerciseFive primes)