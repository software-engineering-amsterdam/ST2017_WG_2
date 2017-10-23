-- About 45 minutes

-- Here, we test for the first 1000 Carmichael numbers,
-- how many numbers can fool Miller-Rabin for certain k.
-- This process is repeated 100 times to get an average number.

-- For k=1, on average 102 numbers fool the test.
-- For k=2, on average 11 numbers fool the test.

-- For k=3, on average 1.32 numbers fool the test.
   -- Selection of numbers that sometimes fool the test:
   -- [2028691238689,34145471279535409,52074923811274729]

-- For k=4, on average 0.15 numbers fool the test.
-- For k=5, on average 0.02 numbers fool the test.

-- For k=6 and higher,
-- none of the first 1000 carmichael numbers fools the Miller-Rabin primality check.
-- The results show that the Miller-Rabin primality check is a lot more trustworthy than
-- the Fermat primality check, as it is fooled a lot less. This is also stated by the Wikipedia page on
-- the Fermat primality check.

module Ex6 where

import Data.List
import System.Random
import Lecture6

carmichael :: [Integer]
carmichael = [(6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1)]

foolMRC :: Int -> Int -> [Integer] -> IO [Integer]
foolMRC k n z = if n == 1000 then return z else do
    pt <- primeMR (fromIntegral k) c
    if pt then foolMRC k (n+1) (z++[c]) else foolMRC k (n+1) z
    where c = carmichael!!n

-- for the first 1000 carmichael numbers,
-- test how many numbers fool Miller-Rabin for certain k.
foolCount :: Int -> Int -> Int -> IO Int
foolCount k n nf = if n == 1000 then return nf
                    else do
                    pt <- primeMR (fromIntegral k) c
                    if pt then foolCount k (n+1) (nf+1) else foolCount k (n+1) nf
                    where c = carmichael!!n

-- Average count of the first 1000 carmichael numbers fooling the
-- Miller-Rabin test over 100 runs.
averageFoolCount :: Int -> Int -> Int -> IO Float
averageFoolCount k n z = if n == 100 then return (fromIntegral z / fromIntegral n) else do
    f <- foolCount k 0 0
    averageFoolCount k (n+1) (z+f)


-- main = foolMRC 3 0 []
main = averageFoolCount 2 0 0