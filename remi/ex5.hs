-- Started at 17:10, finished at 17:25

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
exerciseFive x = if prime xs then xs else exerciseFive (tail x)
    where xs = sum (take 101 x)

main = exerciseFive primes