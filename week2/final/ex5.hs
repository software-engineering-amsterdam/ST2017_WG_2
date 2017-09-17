-- Took about 2 hours

-- Properties:
-- Derangements are not reflexive, as a list can not be it's own derangement
-- Derangements are symmetric, if a is a derangement of b, b is also a derangement of a

-- Testing:
-- The irreflexive property can be tested using QuickCheck, as it only needs one list input
-- The symmetric property is hard to test using QuickCheck, as it barely generates parameters (a,b)
-- for which (isDerangement a b) == True
-- Therefore, we also tested it using some test cases defined by our own

-- Property strength:
-- We did not understand how to use the stronger/weaker functions on
-- functions with different numbers of parameters.

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Permutations function from exercise 4
deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst x (b:bc)
    | x == b = bc
    | otherwise = b : deleteFirst x bc

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] b = b == []
isPermutation a b
    | length a /= length b = False
    | otherwise = isPermutation xs (deleteFirst x b)
    where x = head a
          xs = tail a

-- Check if two lists have any matching elements
hasMatch :: Eq a => [a] -> [a] -> Bool
hasMatch [] [] = False
hasMatch (x:xs) (y:ys) = x == y || hasMatch xs ys

-- First, check if a and b are permutations. Next,
-- check if no elements match positions in the list.
isDerangement :: [Int] -> [Int] -> Bool
isDerangement a b = a /= b && isPermutation a b && not (hasMatch a b)

-- Generate permutations of [0..n-1], then filter out permutations that
-- have values at the same position as [0..n-1].
deran :: Int -> [[Int]]
deran n = filter (\x -> not (hasMatch x a)) (permutations a)
    where a = [0..n-1]

-- Properties
propIrreflexive :: [Int] -> Bool
propIrreflexive a = not (isDerangement a a)

propSymmetric :: [Int] -> [Int] -> Bool
propSymmetric a b = isDerangement a b --> isDerangement b a

main = do
    -- Only need one list, so can use QuickCheck
    quickCheck propIrreflexive

    -- Quickcheck barely generates parameters (a,b) for which (isDerangement a b) == True
    quickCheck propSymmetric

    -- Therefore, some own tests
    print (propSymmetric [1,2,3] [3,1,2])
    print (propSymmetric [1,2,3,4] [4,3,2,1])
    print (propSymmetric [1,2,3,4] [3,4,1,2])
    print (propSymmetric [1..12] [12,11..1])