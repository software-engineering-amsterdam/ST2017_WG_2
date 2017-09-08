-- Started at 16:15
module Lab1 where
import Data.List
import Test.QuickCheck

-- Functions from the lab exercise page
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

sumP :: [Integer] -> Integer
sumP [] = 0
sumP (h:t) = if h < 2000000 then h + sumP t else 0

main = sumP primes