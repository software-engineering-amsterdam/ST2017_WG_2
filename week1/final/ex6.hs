-- Question: What is the smallest counterexample?
-- Answer:
-- in the serie: [2,3,5,7,11,13]
-- the sum is: 30031
-- which is not a prime number
-- The smallest counterexample is when n = 6
--
-- This took ~30 minutes

import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

isProductPlusOnePrime :: [Integer] -> Bool
isProductPlusOnePrime p = prime ((foldl (*) 1 p) + 1)

primeConjecture :: Int -> [Integer]
primeConjecture n
    | isProductPlusOnePrime p = primeConjecture (n + 1)
    | otherwise = p
    where p = take n primes

main = do
    let match = primeConjecture 1
    let productPlusOne = (foldl (*) 1 match) + 1
    print (match)
    print (productPlusOne)
    print (prime productPlusOne)