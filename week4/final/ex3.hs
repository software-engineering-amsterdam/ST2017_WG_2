-- About 1 hour

module Ex3 where

import Data.List
import Test.QuickCheck
import SetOrd
import Ex2

-- Union is not implemented since it is already present in SetOrd.hs.

-- Set intersection
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) set2 = emptySet
intersectSet (Set (x:xs)) set2
    | inSet x set2 = insertSet x (intersectSet (Set xs) set2)
    | otherwise = intersectSet (Set xs) set2

-- Set difference
diffSet :: (Ord a) => Set a -> Set a -> Set a
diffSet (Set []) set2  = emptySet
diffSet (Set (x:xs)) set2
    | inSet x set2 = diffSet (Set xs) set2
    | otherwise = insertSet x (diffSet (Set xs) set2)

set2list :: Set a -> [a]
set2list (Set []) = []  
set2list (Set (x:xs)) = [x] ++ (set2list (Set xs))

checkPropertiesForSet :: Set Int -> Set Int -> Bool
checkPropertiesForSet set1 set2 = 
    subSet set1 set1 -- a set must always be a subset of itself
    && subSet set1 (unionSet set1 set1) && subSet (unionSet set1 set1) set1 -- the union of itself must be a subset of itself, and vice versa
    && all (\x -> (inSet x set1) || (inSet x set2)) (set2list (unionSet set1 set2)) -- each element x in the union between a and b must be in a and b
    && all (\x -> (inSet x set1) /= (inSet x set2)) (set2list (diffSet set1 set2))   -- each element x in the difference between a and b must only be in of [a, b]
    && all (\x -> (inSet x set1) && (inSet x set2)) (set2list (intersectSet set1 set2)) -- each element x in the intersection between a and b must be in a and b
    && all (\x -> not (inSet x (diffSet set1 set2))) (set2list (intersectSet set1 set2)) -- all elements of the difference must not be in the intersection
    && all (\x -> not (inSet x (intersectSet set1 set2))) (set2list (diffSet set1 set2)) -- all elements of the intersection must not be in the difference

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
    -- Own generator test
    ownTest checkPropertiesForSet
    -- QuickCheck test
    quickCheck checkPropertiesForSet
