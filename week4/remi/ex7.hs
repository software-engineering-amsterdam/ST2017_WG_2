-- About 1 hour

-- A random Rel generator is used for generating test cases.
-- However, we can also very simple just use QuickCheck,
-- as a Rel is just an array of tuples of Ords.
-- Using a format like quickCheck (propTcSubset::(Rel Int) -> Bool),
-- QuickCheck knows what datatype to use for the relation.
-- This way, we can also quickly change the datatypes used for testing.

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
import Control.Monad
import Test.QuickCheck
import Ex5
import Ex6

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

isSymmetric :: Ord a => Rel a -> Bool
isSymmetric r = all (\(x, y) -> (y, x) `elem` r) r

-- Random Rel generator --

randomTuple :: IO(Int, Int)
randomTuple = do
    n <- randomRIO (0, 10)
    k <- randomRIO (0, 10)
    return (n, k)

randomRel :: Int -> IO((Rel Int))
randomRel n = replicateM n randomTuple

getRandomRel :: IO((Rel Int))
getRandomRel = do
    n <- randomRIO (1, 30)
    randomRel n

-- Own test function, using method from lecture 2 slides
testR :: Int -> Int -> ((Rel Int) -> Bool) -> IO ()
testR k n f = if k == n then print (show n ++ " tests passed")
                else do
                xs <- getRandomRel
                if f xs then
                    testR (k+1) n f
                else error ("failed test on: \n" ++ show xs)

ownTest :: ((Rel Int) -> Bool) -> IO ()
ownTest f = testR 1 100 f


-- Symmetric closure --

propScSubset :: Ord a => Rel a -> Bool
propScSubset r = all (\x -> x `elem` sc) r
    where sc = symClos r

propScSymmetric :: Ord a => Rel a -> Bool
propScSymmetric r = isSymmetric sc
    where sc = symClos r

propRSymmetric :: Ord a => Rel a -> Bool
propRSymmetric r = isSymmetric r --> sort r == sc
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
    print "--Symmetric closure (own test method)--"
    ownTest propScSubset
    ownTest propScSymmetric
    ownTest propRSymmetric
    ownTest propScSc
    ownTest propScSorted

    putStr("\n")
    print "--Symmetric closure (QuickCheck)--"
    quickCheck (propScSubset::(Rel Int) -> Bool)
    quickCheck (propScSymmetric::(Rel Int) -> Bool)
    quickCheck (propRSymmetric::(Rel Int) -> Bool)
    quickCheck (propScSc::(Rel Int) -> Bool)
    quickCheck (propScSorted::(Rel Int) -> Bool)

    putStr("\n")
    print "--Transitive closure (QuickCheck)--"
    quickCheck (propTcSubset::(Rel Int) -> Bool)
    quickCheck (propTcInterSect::(Rel Int) -> (Rel Int) -> Bool)
    quickCheck (propTcTc::(Rel Int) -> Bool)
    quickCheck (propTcSorted::(Rel Int) -> Bool)