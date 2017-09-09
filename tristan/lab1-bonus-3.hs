-- Matches found: [(1487,4817,8147),(2969,6299,9629)]
-- since (1487,4817,8147) was known, the answer is (2969,6299,9629)
-- resulting in 296962999629
--
-- This took ~1.5 hours

import Data.List

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

digits :: Integer -> [Integer]
digits = map (read . return) . show

uniqueProperty :: Integer -> Integer -> Integer -> Bool
uniqueProperty a b c = 
    a < 10000 && b < 10000 && c < 10000 && (a /= b && b /= c) && (c - b == b - a) && prime a && prime b && prime c && arePermutations a b c

-- Lists are a permutation if their sorted representation matches
-- This does not account for equal input, that should be caught somewhere else
arePermutations :: Integer -> Integer -> Integer -> Bool
arePermutations a b c = do
    let adigits = sort (digits a)
    let bdigits = sort (digits b)
    let cdigits = sort (digits c)
    adigits == bdigits && bdigits == cdigits

uniqueProperties :: [(Integer, Integer, Integer)]
uniqueProperties = [(a, b, c) | a <- [1000..10000], b <- [a..10000], c <- [b + (b - a)], uniqueProperty a b c] 

main = do
    print (uniqueProperties)