-- ID: 11408227
-- Name: Vincent Jong
-- Time: 15:00 -  (Programming), (Testing), (Answering)

module Ex3 where

import Data.List
import Test.QuickCheck
import System.Random
import SetOrd
import Ex2

intersectionSet :: Set Int -> Set Int -> Set Int
intersectionSet (Set []) (Set []) = emptySet
intersectionSet (Set []) s2 = emptySet
intersectionSet s1 (Set []) = emptySet
intersectionSet (Set (s:s1)) s2 = if inSet s s2 
    then unionSet (insertSet s emptySet) (intersectionSet (Set s1) s2) 
    else intersectionSet (Set s1) s2

differenceSet :: Set Int -> Set Int -> Set Int
differenceSet (Set s1) (Set s2) = do
    let combined = list2set (nub (s1 ++ s2))
    let intersection = intersectionSet (Set s1) (Set s2)
    deleteSets intersection combined

deleteSets :: Set Int -> Set Int -> Set Int
deleteSets (Set []) s2 = s2
deleteSets (Set (s:s1)) s2 = do
    let nlist = deleteSet s s2
    deleteSets (Set s1) nlist

-- Properties for intersectionSet

-- Property that all elements of an intersection must be in both original sets (assumes intersection is calculated properly)
propI1 :: Set Int -> Set Int -> Bool
propI1 s1 s2 = all (\elem -> inSet elem s1 && inSet elem s2) inter
    where (Set inter) = intersectionSet s1 s2 

-- Same property as propI1 but checks manually if each common element in s1 and s2 are also in the intersection
propI1c :: Set Int -> Set Int -> Bool
propI1c (Set []) (Set []) = True
propI1c (Set []) s2 = True
propI1c s1 (Set []) = True
propI1c (Set (s:s1)) s2 = do
    let inter = intersectionSet (insertSet s (Set s1)) s2
    if inSet s s2 then inSet s inter && propI1c (Set s1) s2 else propI1c (Set s1) s2

-- Properties for differenceSet

-- Property that the union of s1 and s2 minus the intersection should be the difference set
propD1 :: Set Int -> Set Int -> Bool
propD1 (Set s1) (Set s2) = diff == t
    where
        combined = list2set (nub (s1 ++ s2))
        inter = intersectionSet (Set s1) (Set s2)
        diff = differenceSet (Set s1) (Set s2)
        t = deleteSets inter combined

-- Property that all elements in the difference set should only belong to one of the original sets
--propD2 :: Set Int -> Set Int -> Bool
--propD2 s1 s2 = do
--    let 

main = do
    s1 <- randomList
    s2 <- randomList
    print s1
    print s2
    print $ intersectionSet s1 s2
    print $ differenceSet s1 s2
    print $ propI1 s1 s2
    print $ propI1c s1 s2
    print $ propD1 s1 s2