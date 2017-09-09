-- Started at 9:00, finished at 9:20

-- If we accept 0 for n, n=0 the smallest counter example: the product of 0 primes is 0, 0 + 1 = 1, which is not a prime number.
-- If we do not accept 0 for n, the smallest counter example is n=6.

module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Prime functions from the lab exercise page
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

exerciseSix :: Integer -> Bool
exerciseSix n = prime (p + 1)
    where xs = take (fromIntegral n) primes
          p = if n == 0 then 0 else product xs

main = quickCheck (\ x -> (x >= 0) --> exerciseSix x)