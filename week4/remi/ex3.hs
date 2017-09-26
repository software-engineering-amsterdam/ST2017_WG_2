-- About 2 hours

-- Union properties:
-- All values in the union should be in one of the unified lists
-- However, this is not enough to test, as the union could still miss values
-- Therefore, two other properties:
-- all values from list 1 should be in the union
-- all values from list 2 should be in the union

-- Intersection properties:
-- All values in the intersection should be in both of the intersected lists
-- Again, this is not enough to test, as the intersection could still miss values
-- Therefore, another property:
-- all values from list 1 that are also in list 2 should be in the intersection

-- Difference properties:
-- All values in the difference should be only in list 1
-- Again, this is not enough to test, as the difference could still miss values
-- Therefore, another property:
-- all values from list 1 that are not in list 2 should be in the difference

-- First, the properties are tested using 100 random sets generated using our own set
-- generator from exercise 2.

-- Next, the properties are tested using QuickCheck using our QuickCheck set
-- generator from exercise 2.

module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Ex2

-- Union is in SetOrd.hs already

-- Set intersection
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) set2  = emptySet
intersectSet (Set (x:xs)) set2
    | inSet x set2 = insertSet x (intersectSet (Set xs) set2)
    | otherwise = intersectSet (Set xs) set2

-- Set difference
diffSet :: (Ord a) => Set a -> Set a -> Set a
diffSet (Set []) set2  = emptySet
diffSet (Set (x:xs)) set2
    | inSet x set2 = diffSet (Set xs) set2
    | otherwise = insertSet x (diffSet (Set xs) set2)

-- Union properties
propUnion :: Set Int -> Set Int -> Bool
propUnion l1 l2 = all (\x -> inSet x l1 || inSet x l2) union
    where (Set union) = unionSet l1 l2

propUnion2 :: Set Int -> Set Int -> Bool
propUnion2 (Set l1) l2 = all (\x -> inSet x union) l1
    where union = unionSet (Set l1) l2

propUnion3 :: Set Int -> Set Int -> Bool
propUnion3 l1 (Set l2) = all (\x -> inSet x union) l2
    where union = unionSet l1 (Set l2)

-- Intersection properties
propIntersect :: Set Int -> Set Int -> Bool
propIntersect l1 l2 = all (\x -> inSet x l1 && inSet x l2) intersect
    where (Set intersect) = intersectSet l1 l2

propIntersect2 :: Set Int -> Set Int -> Bool
propIntersect2 (Set l1) l2 = all (\x -> inSet x l2 == inSet x intersect) l1
    where intersect = intersectSet (Set l1) l2

-- Difference properties
propDiff :: Set Int -> Set Int -> Bool
propDiff l1 l2 = all (\x -> inSet x l1 && not(inSet x l2)) diff
    where (Set diff) = diffSet l1 l2

propDiff2 :: Set Int -> Set Int -> Bool
propDiff2 (Set l1) l2 = all (\x -> inSet x l2 /= inSet x diff) l1
    where diff = diffSet (Set l1) l2

-- Own test function, using method from lecture 2 slides
testR :: Int -> Int -> (Set Int -> Set Int -> Bool) -> IO ()
testR k n f = if k == n then print (show n ++ " tests passed")
                else do
                xs <- randomSet
                ys <- randomSet
                if f xs ys then
                    testR (k+1) n f
                else error ("failed test on: \n" ++ show xs ++ "\n" ++ show ys)

ownTest :: (Set Int -> Set Int -> Bool) -> IO ()
ownTest f = testR 1 100 f

main = do
    print "-- Union --"
    ownTest propUnion
    quickCheck propUnion
    ownTest propUnion2
    quickCheck propUnion2
    ownTest propUnion3
    quickCheck propUnion3

    putStr "\n"
    print "-- Intersection --"
    ownTest propIntersect
    quickCheck propIntersect
    ownTest propIntersect2
    quickCheck propIntersect2

    putStr "\n"
    print "-- Difference --"
    ownTest propDiff
    quickCheck propDiff
    ownTest propDiff2
    quickCheck propDiff2
