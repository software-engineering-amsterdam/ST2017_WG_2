-- Started at 16:10

-- Here, we test for the first 1000 Carmichael numbers,
-- how many numbers can fool Fermat for certain k.

-- For k=1, on average all 1000 numbers fool Fermat.
-- For k=2, on average all 1000 numbers fool Fermat.
-- For k=3, on average all 1000 numbers fool Fermat.
-- For k=50, on average 996 numbers fool Fermat.
-- For k=500, on average 974 numbers fool Fermat.

-- This shows that Carmichael numbers almost always fool Fermat.

module Ex5 where

import Data.List
import System.Random
import Lecture6

carmichael :: [Integer]
carmichael = [(6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1)]

foolFermatC :: Int -> Int -> IO Integer
foolFermatC k n = do
    pt <- primeTestsF (fromIntegral k) c
    if pt then return c else foolFermatC k (n+1)
    where c = carmichael!!n

-- Iterate foolFermatC 1000 times to have a good chance of finding the least
-- composite number that fools the check.
findSmallest :: Int -> Int -> [Integer] -> IO Integer
findSmallest k n z = if n == 1000 then return $ head $ sort z
                else do
                p <- foolFermatC k 0
                findSmallest k (n + 1) (z++[p])

-- test for the first 1000 carmichael numbers,
-- how many numbers can fool Fermat for certain k.
foolPercentage :: Int -> Int -> Int -> IO Int
foolPercentage k n nf = if n == 1000 then return nf
                    else do
                    pt <- primeTestsF (fromIntegral k) c
                    if pt then foolPercentage k (n+1) (nf+1) else foolPercentage k (n+1) nf
                    where c = carmichael!!n

main = foolPercentage 500 0 0
-- main = findSmallest 50 0 []