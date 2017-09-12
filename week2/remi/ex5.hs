-- Took about 2 hours

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Functions to check property strength
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q
                    qp = stronger xs q p
                in
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"

-- Deletes first occurence of a value from a list
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

hasMatch :: Eq a => [a] -> [a] -> Bool
hasMatch [] [] = False
hasMatch (x:xs) (y:ys) = x == y || hasMatch xs ys

-- First, check if a and b are permutations. Next,
-- check if no elements match positions in the list.
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b = a /= b && isPermutation a b && not (hasMatch a b)

-- Generate permutations of [0..n-1], then filter out permutations that
-- have values at the same position as [0..n-1].
deran :: Int -> [[Int]]
deran n = filter (\x -> not (hasMatch x a)) (permutations a)
    where a = [0..n-1]

-- Properties
propIrreflexive :: Ord a => [a] -> Bool
propIrreflexive a = not (isDerangement a a)

propSymmetric :: Ord a => [a] -> [a] -> Bool
propSymmetric a b = isDerangement a b --> isDerangement b a

propPermutation :: Ord a => [a] -> [a] -> Bool
propPermutation a b = isDerangement a b --> isPermutation b a

-- Tests
testIrreflexive :: [Integer] -> Bool
testIrreflexive a = propIrreflexive a

testSymmetric :: [Integer] -> [Integer] -> Bool
testSymmetric a b = propSymmetric a b

testPermutation :: [Integer] -> [Integer] -> Bool
testPermutation a b = propPermutation a b

main = do
    quickCheck testIrreflexive

    -- Quickcheck barely generates paremeters (a,b) for which (isDerangement a b) == True
    quickCheck testSymmetric
    quickCheck testPermutation

    -- Therefore, some own tests
    print (testSymmetric [1,2,3] [3,1,2])
    print (testSymmetric [1,2,3,4] [4,3,2,1])
    print (testSymmetric [1..12] [12,11..1])

    print (testPermutation [1,2,3] [3,1,2])
    print (testPermutation [1,2,3,4] [4,3,2,1])
    print (testPermutation [1..12] [12,11..1])