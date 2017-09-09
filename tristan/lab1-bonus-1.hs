-- This took ~1 hour

import Data.List

isPythagoreanTriplet :: Integer -> Integer -> Integer -> Bool
isPythagoreanTriplet a b c = a < b && b < c && a^2 + b^2 == c^2

pythagoreanTriplets :: [(Integer, Integer, Integer)]
pythagoreanTriplets = [(a, b, c) | c <- [0..], b <- [0..c-1], a <- [0..b-1], isPythagoreanTriplet a b c] 

main = do
    print ("Searching...")
    print (find (\(a, b, c) -> a + b + c == 1000) pythagoreanTriplets)
