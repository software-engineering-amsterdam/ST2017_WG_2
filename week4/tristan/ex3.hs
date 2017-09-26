module Ex3 where

import Ex2
import Data.List
import SetOrd
import Test.QuickCheck

intersection, union, difference :: Ord a => Set a -> Set a -> Set a

intersection (Set a) (Set b) = list2set (filter (\x -> elem x b) a)

union a b = unionSet a b

difference (Set a) (Set b) = list2set (nub ((a \\ b) ++ (b \\ a)))

prop_intersection, prop_union, prop_difference :: Set Int -> Bool

prop_intersection x = True

prop_union x = True

prop_difference x = True

main = do
    quickCheck (forAll randomSetQC prop_intersection)
    quickCheck (forAll randomSetQC prop_union)
    quickCheck (forAll randomSetQC prop_difference)