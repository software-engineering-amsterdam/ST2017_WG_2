module Ex2 (randomSetQC) where

import Control.Monad
import SetOrd
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = randomSetQC

randomSetQC :: (Ord a, Arbitrary a) => Gen (Set a)
randomSetQC = do
    list <- listOf arbitrary
    return (list2set list)

main = do
    set <- generate (randomSetQC::Gen (Set Int))
    print (set)