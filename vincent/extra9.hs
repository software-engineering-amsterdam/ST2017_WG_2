-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~1 hr

import Test.QuickCheck
import Data.List

range = [ x | x <- subsequences [0..100], length x == 3 ]

isPythagorean :: Int -> Int -> Int -> Bool
isPythagorean x y z = (x ^ 2 + y ^ 2) == z ^ 2

isPythTriplet :: Int -> Int -> Int -> Bool
isPythTriplet x y z = isPythagorean x y z && x < y && y < z

-- test for [3,4,5] with 12, product should be 60
--findPythTriplet :: [[Int]] -> Int
--findPythTriplet [] = (-1)
--findPythTriplet (x:xs) = 
--    if isPythTriplet (x !! 0) (x !! 1) (x !! 2) && (sum x) == 1000
--        then product x
--        else findPythTriplet xs

-- Derived the formulas for z and y through mathmetical simplification, since we know x + y + z = 1000 and x^2 + y^2 = z^2
-- Problem with this implementation is that it only works for this example. So it can't find e.g. [3,4,5]
findPythTriplet :: Int -> [Int]
findPythTriplet x =
    do
        let z = (x * x + 500000 - 1000 * x) `div` (1000 - x)
        let y = 1000 - z - x
        if isPythTriplet x y z then [x,y,z] else findPythTriplet (x + 1)

main = print $ product $ findPythTriplet 0