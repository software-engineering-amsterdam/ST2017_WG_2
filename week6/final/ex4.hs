-- We have chosen for this solution from all our solutions because this solution does 1000 
-- iterations and is generally good structured.
-- About 30 minutes

-- Least composite number that you can find that fools the check for k=1:
-- 9

-- Least composite number that you can find that fools the check for k=2:
-- 9

-- Least composite number that you can find that fools the check for k=3:
-- 9

-- By repeating foolFermat by hand, finding these values would be a lot harder,
-- as the larger the value for k, the larger values that fool fermat are returned.

-- This can be explained as follows:
-- Doing the Fermat-test with more random test cases,
-- the chance that at least one of the test cases returns false increases,
-- so the Fermat function returns false as well.
-- The same way, using only a few test cases means that there is a bigger chance that all test
-- cases return true, so the Fermat function returns true as well (and is therefore fooled).

module Ex4 where

import Data.List
import System.Random
import Lecture6

foolFermat :: Int -> Int -> IO Integer
foolFermat k n = do
    pt <- primeTestsF (fromIntegral k) c
    if pt then return c else foolFermat k (n+1)
    where c = composites!!n

-- Iterate foolFermat 1000 times to have a good chance of finding the least
-- composite number that fools the check.
findSmallest :: Int -> Int -> [Integer] -> IO Integer
findSmallest k n z = if n == 1000 then return $ head $ sort z
                else do
                p <- foolFermat k 0
                findSmallest k (n + 1) (z++[p])

main = findSmallest 1 0 []