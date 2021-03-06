-- Started at 16:30, finished at 17:05 (programming part)

module Lab1 where
import Data.List
import Test.QuickCheck

-- Functions from the lab exercise page
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show

exerciseFour :: [Integer]
exerciseFour = filter( \d -> prime d && prime (reversal d)) [10..10000]

main = exerciseFour