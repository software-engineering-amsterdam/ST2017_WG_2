-- ID: 11408227
-- Name: Vincent Jong
-- Time: 15:00 - 16:45, 9:15 - 11:15 (Programming), (Testing), (Answering)

-- Intersection is created by checking for each element in a set if it also in the other set.
-- If so, this element is put in a new set. If not, skip this element and move onto next element.
-- Tested intersection by checking whether each element in the intersection is an element of both of the original sets.
-- Both my own test generator and quickCheck reported no errors.

-- Difference set is created by first unifying the 2 sets (removing duplicates). Then the intersection is calculated
-- and then subtracted from the union. Another possible approach is by checking for each element of a set if it does
-- not exist in the other set and putting that in another set. But this has to be done both ways (s1 -> s2 and s2 -> s1).
-- The properties I checked is (1) that all elements in the difference set can only belong to one of the original sets
-- and (2) checking if the difference is indeed the union of the 2 sets minus the intersection.
-- Both my own test generator and quickCheck reported no errors.

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

-- Same property as propI1 but checks manually if each common element in s1 and s2 is also in the intersection
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
propD2 :: Set Int -> Set Int -> Bool
propD2 s1 s2 = all (\elem -> if inSet elem s1 then not (inSet elem s2) else inSet elem s2) diff
    where (Set diff) = differenceSet s1 s2

-- testR from Lecture 2
testR :: Int -> Int -> (Set Int -> Set Int -> Bool) -> IO ()
testR k n f = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- randomList
                  ys <- randomList
                  if f xs ys then
                    --do print ("pass on: " ++ show xs)
                       testR (k+1) n f
                  else error ("failed test on: " ++ show xs)

testProp :: (Set Int -> Set Int -> Bool) -> IO()
testProp f = testR 1 100 f

main = do
    testProp propI1
    quickCheck propI1

    testProp propI1c
    quickCheck propI1c

    testProp propD1
    quickCheck propD1

    testProp propD2
    quickCheck propD2