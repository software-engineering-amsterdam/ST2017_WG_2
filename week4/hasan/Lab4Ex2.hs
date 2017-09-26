-- Start time 14:05, end time: 15:15

module Lab4Ex2 where

import Data.List
import System.Random
import Control.Monad
import Test.QuickCheck
import SetOrd


--- Generate a set without QuickCheck
randomNumber :: Int -> Int -> IO Int
randomNumber min max = do
	number <- getStdRandom (randomR (min, max))
	return number

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
    p <- randomNumber 0 100
    ps <- randomList (n-1)
    return (p:ps)
	
randomSet :: IO (Set Int)
randomSet = do
	size <- randomNumber 0 100
	list <- randomList size
	return (list2set list)

ex2a = do
	set <- randomSet
	print set

--- Generate a set with QuickCheck
arbitrarySet :: Gen (Set Int)
arbitrarySet = do
	a <- arbitrary
	return (list2set a)

ex2b = do
	a <- generate arbitrarySet
	print a