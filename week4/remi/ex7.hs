-- About 1 hour

-- We can use QuickCheck, as a Rel is just an array of tuples of Ords.
-- Using a format like quickCheck (propTcSubset::(Rel Int) -> Bool),
-- QuickCheck knows what datatype to use for the relation.
-- Therefore, we do not need to build an own arbritrary Rel generator
-- like for Sets in exercise 2.

-- Symmetric closure properties:
-- R is a subset of symClos R
-- symClos R is symmetric
-- if R is symmetric, symClos R = R
-- the symmetric closure of a symmetric closure is the same
-- the symmetric closure should be ordered

-- Transitive closure properties:
-- R is a subset of trClos R
-- intersection of two transitive closures is transitive
-- transitive closure of transitive closure is the same
-- the transitive closure should be ordered

module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Ex5
import Ex6

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

isSymmetric :: Ord a => Rel a -> Bool
isSymmetric r = all (\(x, y) -> (y, x) `elem` r) r

-- Symmetric closure --

propScSubset :: Ord a => Rel a -> Bool
propScSubset r = all (\x -> x `elem` sc) r
    where sc = symClos r

propScSymmetric :: Ord a => Rel a -> Bool
propScSymmetric r = isSymmetric sc
    where sc = symClos r

propRSymmetric :: Ord a => Rel a -> Bool
propRSymmetric r = isSymmetric r --> r == sc
    where sc = symClos r

propScSc :: Ord a => Rel a -> Bool
propScSc r = symClos sc == sc
    where sc = symClos r

propScSorted :: Ord a => Rel a -> Bool
propScSorted r = sort sc == sc
    where sc = symClos r

-- Transitive closure--

propTcSubset :: Ord a => Rel a -> Bool
propTcSubset r = all (\x -> x `elem` tc) r
    where tc = trClos r

propTcInterSect :: Ord a => Rel a -> Rel a -> Bool
propTcInterSect r rs = transR tcss
    where tc = trClos r
          tcs = trClos rs
          tcss = filter (\x -> x `elem` tcs) tc

propTcTc :: Ord a => Rel a -> Bool
propTcTc r = trClos tc == tc
    where tc = trClos r

propTcSorted :: Ord a => Rel a -> Bool
propTcSorted r = sort tc == tc
    where tc = trClos r

main = do
    print "--Symmetric closure--"
    quickCheck (propScSubset::(Rel Int) -> Bool)
    quickCheck (propScSymmetric::(Rel Int) -> Bool)
    quickCheck (propRSymmetric::(Rel Int) -> Bool)
    quickCheck (propScSc::(Rel Int) -> Bool)
    quickCheck (propScSorted::(Rel Int) -> Bool)

    putStr("\n")
    print "--Transitive closure--"
    quickCheck (propTcSubset::(Rel Int) -> Bool)
    quickCheck (propTcInterSect::(Rel Int) -> (Rel Int) -> Bool)
    quickCheck (propTcTc::(Rel Int) -> Bool)
    quickCheck (propTcSorted::(Rel Int) -> Bool)