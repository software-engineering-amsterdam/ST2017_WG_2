-- This took ~15 minutes

module Lab2_4 (isPermutation) where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lab2_3

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = isEqualLength x y && isEmptyList (dropDuplicates x y)
 
dropDuplicates :: Eq a => [a] -> [a] -> [a]
dropDuplicates x y = x \\ y

isEmptyList :: Eq a => [a] -> Bool
isEmptyList x = x == []

isEqualLength :: Eq a => [a] -> [a] -> Bool
isEqualLength x y = length x == length y 

prop_identity, prop_firstReversed, prop_secondReversed, prop_firstSorted, prop_permutation :: (Eq a, Ord a) => [a] -> Bool
prop_identity x =isPermutation x x
prop_firstReversed x = isPermutation (reverse x) x
prop_secondReversed x = isPermutation x (reverse x)
prop_firstSorted x = isPermutation (sort x) x
prop_secondSorted x = isPermutation x (sort x)
prop_permutation x = prop_identity x && prop_firstReversed x && prop_secondReversed x && prop_firstSorted x && prop_secondSorted x

main = do
    print (isPermutation ([] :: [Int]) ([] :: [Int]) == True)
    print (isPermutation [1] [1] == True)
    print (isPermutation [1, 2, 3] [3, 2, 1] == True)
    print (isPermutation [3, 2, 1] [1, 2, 3] == True)
    print (isPermutation [(-1), (-2), (-3)] [(-3), (-2), (-1)] == True)
    print (isPermutation [] [1, 2, 3] == False)
    print (isPermutation [1, 2, 3] [] == False)
    print (isPermutation [1, 2, 3] [4, 5, 6] == False)
    print (isPermutation [1, 2] [1, 2, 3] == False)
    print (isPermutation [1, 2, 4] [1, 2, 3] == False)

    let perms = permutations [(-3)..3]
    
    let properties = [ NamedProperty {name = "prop_identity", prop = prop_identity}
                     , NamedProperty {name = "prop_firstReversed", prop = prop_firstReversed}
                     , NamedProperty {name = "prop_secondReversed", prop = prop_secondReversed}
                     , NamedProperty {name = "prop_firstSorted", prop = prop_firstSorted} ]

    print (sortStrength perms properties)

    quickCheck (prop_permutation :: [Integer] -> Bool)
