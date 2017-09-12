-- This took ~15 minutes

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = isEqualLength x y && dropDuplicates x y == [] 
 
dropDuplicates :: Eq a => [a] -> [a] -> [a]
dropDuplicates x y = x \\ y

isEqualLength :: Eq a => [a] -> [a] -> Bool
isEqualLength x y = length x == length y

main = do
	print (isPermutation [1, 2, 3] [2, 5, 1])
