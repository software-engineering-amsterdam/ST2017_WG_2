-- This took ~15 minutes

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

isPermutation :: [Int] -> [Int] -> Bool
isPermutation x y = sort (x) == sort (y)

isDerangement :: [Int] -> [Int] -> Bool
isDerangement x y = isPermutation x y && all (\(a, b) -> a /= b) (zip x y)

main = do
    print (isDerangement [1, 2, 3] [2, 3, 1])
