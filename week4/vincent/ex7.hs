-- ID: 11408227
-- Name: Vincent Jong
-- Time: 13:10 - 14:15 (Programming), (Testing), (Answering)

module Ex7 (genRel) where

import Data.List
import Test.QuickCheck
import System.Random
import SetOrd
import Ex3
import Ex5
import Ex6

type Rel a = [(a, a)]

-- instance (Arbitrary a, Ord a) => Arbitrary (Rel a) where
--     arbitrary = do
--         l <- genRel 5
--         return l

genRel :: Int -> IO (Rel Int)
genRel 0 = return []
genRel x = do
    rv1 <- randomRIO (0, 9)
    rv2 <- randomRIO (0, 9)
    rl <- genRel (x - 1)
    return ([(rv1, rv2)] ++ rl)

-----------------------------------------------------------------------------------------
-- Properties symClos

-- From Lecture 2
parity n = mod n 2

-- Property that number of elements is even since we're doubling the size of the relation
propSC1 :: (Rel Int) -> Bool
propSC1 rel = parity (length sym) == 0
    where sym = symClos rel

-----------------------------------------------------------------------------------------
-- Properties trClos

-- Property that the intersection of 2 transitive relations is also transitive
propTC1 :: (Ord a) => (Rel a) -> (Rel a) -> Bool
propTC1 rel1 rel2 = isTrans inter
    where (Set inter) = intersectionSet (Set rel1) (Set rel2)

-- Property that converse of a transitive relation is also transitive

-----------------------------------------------------------------------------------------
-- Test generator

-- testR from Lecture 2
testR :: Int -> Int -> (Rel Int -> Bool) -> IO ()
testR k n f = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genRel 5
                  if f xs then
                       testR (k+1) n f
                  else error ("failed test on: " ++ show xs)

testProp :: (Rel Int -> Bool) -> IO()
testProp f = testR 1 100 f

testR2 :: Int -> Int -> (Rel Int -> Rel Int -> Bool) -> IO ()
testR2 k n f = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genRel 5
                  ys <- genRel 5
                  if f xs ys then
                       testR2 (k+1) n f
                  else error ("failed test on: " ++ show xs)

testProp2 :: (Rel Int -> Rel Int -> Bool) -> IO()
testProp2 f = testR2 1 100 f

main = do
    testProp propSC1

    testProp2 propTC1
