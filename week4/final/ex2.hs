-- 1 hour and 10 minutes

-- We chose this generator because it is able to produce a list of random length and random integers that are possibly negative.
-- The outcomes resemble those of the QuickCheck generator. Other implementations only produced positive integers.

module Ex2 where

import Data.List
import System.Random
import Control.Monad
import Test.QuickCheck
import SetOrd

-- QuickCheck random set generator --
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        ls <- listOf (arbitrary)
        return (list2set ls)

-- From scratch random set generator --

-- List of size n with values in range -k to k
randomList :: Int -> Int -> IO([Int])
randomList n k = replicateM n (randomRIO (-k, k))

-- Random list size and range
getRandomList :: IO([Int])
getRandomList = do
    n <- randomRIO (1, 30)
    k <- randomRIO (1, 30)
    randomList n k

-- Generate Set from random list
randomSet :: IO (Set Int)
randomSet = do
    l <- getRandomList
    return (list2set l)

main2 :: IO ()
main2 = do
    s <- randomSet
    print s
