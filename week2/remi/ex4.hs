-- Took about 3 hours

-- Question: You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?
-- This means that we can test using ranges (with different, start, step and end value)
-- and don't have to test with lists containing the same value mutliple times.

-- All properties have the same strength, as all are about permutations, only
-- different kinds of permutations.
-- Therefore, the list of properties sorted by strength can be in any order.

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

-- First check if both list have the same length. If not, they can't be permutations.
-- Next, take the first element from a and remove it's first occurence in b.
-- Repeat this process until their length differs, which mean they are no permutations,
-- or until a is empty. If a is empty, b should be empty as well for a and b to be permutations.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] b = b == []
isPermutation a b
    | length a /= length b = False
    | otherwise = isPermutation xs (deleteFirst x b)
    where x = head a
          xs = tail a

-- Properties
-- Use different order of the parameters, to show symmetry
propReflexive, propReverse1, propReverse2, propReverseBoth, propSort1, propSort2, propSortBoth :: Ord a => [a] -> Bool
propReflexive a = isPermutation a a
propReverse1 a = isPermutation a (reverse a)
propReverse2 a = isPermutation (reverse a) a
propReverseBoth a = isPermutation (reverse a) (reverse a)
propSort1 a = isPermutation a (sort a)
propSort2 a = isPermutation (sort a) a
propSortBoth a = isPermutation (sort a) (sort a)

-- Other way of writing relational properties, but they need multiple arguments
propSymmetric :: Ord a => [a] -> [a] -> Bool
propSymmetric a b = isPermutation a b --> isPermutation b a

propTransitive :: Ord a => [a] -> [a] -> [a] -> Bool
propTransitive a b c = isPermutation a b && isPermutation b c --> isPermutation a c

-- Test if input meets all above properties
testProperties :: [Int] -> Bool
testProperties a = propReflexive a && propReverse1 a && propReverse2 a && propReverseBoth a && propSort1 a && propSort2 a && propSortBoth a

-- Tests
isTrue :: a -> Bool
isTrue _ = True

checkTrue :: Bool -> Bool
checkTrue a = a

-- Hoare test function for the properties of the isPermutation function
hoareTest :: (a -> Bool) -> (a -> Bool) -> (Bool -> Bool) -> [a] -> Bool
hoareTest precondition f postcondition =
    all (\x -> precondition x --> postcondition (f x))

main = do
    -- Test Hoare test method from lecture using lists of length 0 to 99
    print (hoareTest isTrue testProperties checkTrue [[0..n] | n <- [0..99]])

    -- Test using QuickCheck
    quickCheck testProperties

    -- Property strength --
    print "-- propReflexive vs propReverse1, propReverse2, propReverseBoth, propSort1, propSort2, propSortBoth --"
    print (compar [[1,2,3]] propReflexive propReverse1)
    print (compar [[1,2,3]] propReflexive propReverse2)
    print (compar [[1,2,3]] propReflexive propReverseBoth)
    print (compar [[1,2,3]] propReflexive propSort1)
    print (compar [[1,2,3]] propReflexive propSort2)
    print (compar [[1,2,3]] propReflexive propSortBoth)
