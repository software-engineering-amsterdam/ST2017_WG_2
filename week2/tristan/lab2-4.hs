-- 

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

isPermutation :: (Eq a, Ord a) => [a] -> [a] -> Bool
isPermutation x y = sort (x) == sort (y)
