-- About 2 hours

module Lab4 where

import Data.List
import System.Random
import Control.Monad
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
diffSet (Set []) set2  = set2
diffSet (Set (x:xs)) set2
    | inSet x set2 = diffSet (Set xs) (deleteSet x set2)
    | otherwise = insertSet x (diffSet (Set xs) set2)

-- Property - union: all values in the union should be in one of the unified lists
propUnion :: Set Int -> Set Int -> Bool
propUnion l1 l2 = all (\x -> inSet x l1 || inSet x l2) union
    where (Set union) = unionSet l1 l2

-- Property - union: all values in the intersection should be in both of the intersected lists
propIntersect :: Set Int -> Set Int -> Bool
propIntersect l1 l2 = all (\x -> inSet x l1 && inSet x l2) intersect
    where (Set intersect) = intersectSet l1 l2

-- Property - difference: all values in the difference should be in only one of the lists
propDiff :: Set Int -> Set Int -> Bool
propDiff l1 l2 = all (\x -> inSet x l1 /= inSet x l2) diff
    where (Set diff) = diffSet l1 l2

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

    putStr "\n"
    print "-- Intersection --"
    ownTest propIntersect
    quickCheck propIntersect

    putStr "\n"
    print "-- Difference --"
    ownTest propDiff
    quickCheck propDiff
