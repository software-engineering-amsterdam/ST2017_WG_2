-- Time: 15:30 - 16:30 (about 60 min)

-- The least composite number that I got was 9 with k = 1
-- With k = 2 it was 9 and with k = 3 it was 15

-- Increasing k seems to give on average a larger integer as result and takes longer to compute.
-- Below some examples of manual executions with k = 10
-- *Ex4> doPrimeTest composites 10
-- 29341
-- *Ex4> doPrimeTest composites 10
-- 115921
-- *Ex4> doPrimeTest composites 10
-- 2465
-- *Ex4> doPrimeTest composites 10
-- 6601
-- *Ex4> doPrimeTest composites 10
-- 10585

-- I tried k = 10 for 100 iterations but that takes a long while to complete.
-- For k = 5 the result was 65

-- From this, we can see that composite numbers can fool Fermat's check.

module Ex4 (doPrimeTest) where

import Data.List
import System.Random
import Lecture6
import Ex3
import Ex1

-- Copied from lecture using the more efficient exM from Exercise 1
primeTestsF' :: Int -> Integer -> IO Bool
primeTestsF' k n = do
 as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
 return (all (\ a -> Ex1.exM a (n-1) n == 1) as)

doPrimeTest :: [Integer] -> Int -> IO Integer
doPrimeTest [] iters = return (-666) -- Not really necessary as the list of composites is infinite
doPrimeTest (x:xs) iters = do
    isPrimeAndComp <- primeTestsF' iters x 
    if isPrimeAndComp then return x else doPrimeTest xs iters

getLeastNumber :: Int -> Int -> Int -> [Integer] -> [Integer] -> IO [Integer]
getLeastNumber start end k function result = if start == end
    then return (sort result)
    else do
        nextVal <- doPrimeTest function k
        getLeastNumber (start+1) end k function ([nextVal] ++ result)

main = do
    let k = 1
    l <- getLeastNumber 0 100 k composites []
    print $ head l
    let k2 = 2
    l2 <- getLeastNumber 0 100 k2 composites []
    print $ head l2
    let k3 = 3
    l3 <- getLeastNumber 0 100 k3 composites []
    print $ head l3
    let k4 = 5
    l4 <- getLeastNumber 0 100 k4 composites []
    print $ head l4
