-- ID: 11408227
-- Name: Vincent Jong
-- Time: 14:00 - 15:00 (Programming), (Testing), (Answering)

module Ex2 (randomList, randomListGen, randomList2, randomListGen2) where

import Data.List
import Test.QuickCheck
import System.Random
import SetOrd

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where 
    arbitrary = do
        randList <- listOf arbitrary
        return (list2set randList)

-- Random list generator self
-- Generate a list of random length between 1 and 15 of Ints between 0 and 9

randomListGen :: Int -> IO ([Int])
randomListGen 0 = return []
randomListGen x = do
    randInt <- randomRIO (0, 10)
    randList <- randomListGen (x - 1)
    return (randInt:randList)

randomList = do
    randLength <- randomRIO (1, 20)
    randList <- randomListGen randLength
    return (list2set randList)

-- Random list generator with QuickCheck

randomListGen2 :: Gen [Int]
randomListGen2 = listOf (choose (0, 100000))

randomList2 = do
    randList <- generate randomListGen2
    return (list2set randList)

main = do
    l <- randomList
    print $ l

main2 = do
    l <- randomList2
    print l