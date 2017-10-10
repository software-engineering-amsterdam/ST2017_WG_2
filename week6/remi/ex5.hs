-- About 1 hour

-- Here, we test for the first 1000 Carmichael numbers,
-- how many numbers can fool Fermat for certain k.
-- This process is repeated 100 times to get an average number.

-- For k=1, 999.85 numbers, so almost all of the first 1000, fool Fermat.
-- For k=2, 999.82 numbers, so almost all of the first 1000, fool Fermat.
-- For k=3, 999.68 numbers, so almost all of the first 1000, fool Fermat.
-- For k=5, on average 999.51 numbers fool Fermat.
-- For k=10, on average 998.94 numbers fool Fermat.

-- For k=50, on average 995.73 numbers fool Fermat.
--      Selection of numbers that did not fool fermat (using function foolFermatC):
--      [294409,172947529,216821881,2724933935809]

-- For k=500, on average 977.6 numbers fool Fermat.
--      Selection of numbers that did not fool fermat (using function foolFermatC):
--      [56052361,118901521,172947529,1299963601,2301745249,11346205609,27278026129,1042789205881,3296857440241]

-- This shows that Carmichael numbers almost always fool Fermat.

-- According to Wikipedia,
-- Carmichael numbers are also called Fermat pseudoprimes or absolute Fermat pseudoprimes.
-- A Carmichael number will pass a Fermat primality test to every base b relatively prime to the number,
-- even though it is not actually prime. That explains why the (most) Carmichael numbers pass our
-- Fermat test.

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

foolFermatC :: Int -> Int -> [Integer] -> IO [Integer]
foolFermatC k n z = if n == 1000 then return z else do
    pt <- primeTestsF (fromIntegral k) c
    if not pt then foolFermatC k (n+1) (z++[c]) else foolFermatC k (n+1) z
    where c = carmichael!!n

-- for the first 1000 carmichael numbers,
-- test how many numbers fool Fermat for certain k.
foolCount :: Int -> Int -> Int -> IO Int
foolCount k n nf = if n == 1000 then return nf
                    else do
                    pt <- primeTestsF (fromIntegral k) c
                    if pt then foolCount k (n+1) (nf+1) else foolCount k (n+1) nf
                    where c = carmichael!!n

-- Average count of the first 1000 carmichael numbers fooling the
-- Format test over 100 runs.
averageFoolCount :: Int -> Int -> Int -> IO Float
averageFoolCount k n z = if n == 100 then return (fromIntegral z / fromIntegral n) else do
    f <- foolCount k 0 0
    averageFoolCount k (n+1) (z+f)

-- main = foolFermatC 50 0 []
main = averageFoolCount 10 0 0