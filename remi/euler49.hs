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

-- Using the functions from exercise 7, so we can generate permutations
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

fromDigits :: [Integer] -> Integer
fromDigits x = foldl ((+) . (*10)) 0 x

dListToIList :: [[Integer]] -> [Integer]
dListToIList [] = []
dListToIList (h:t) = [fromDigits h] ++ (dListToIList t)

-- Concat list of integers to string
concatInts :: [Integer] -> String
concatInts [] = ""
concatInts (h:t) = show h ++ concatInts t

-- First, check if prime is not 1487, as we already know that one
-- Next, check if prime + 3330 and prime + 6660 are primes as well
-- Next, check if those two primes are permutations of the first prime
-- Finally, if all checks were true, concatenate those primes and return the result
euler :: [Integer] -> Integer
euler (p:t) = if p /= 1487 && prime p2 && prime p3 && p2p && p3p then c else euler t
    where p2 = p + 3330
          p3 = p2 + 3330
          pm = dListToIList (permutations (toDigits p))
          p2p = p2 `elem` pm
          p3p = p3 `elem` pm
          c = read (concatInts [p, p2, p3])

main = euler (filter (>= 1000) (takeWhile (<= 9999) primes))