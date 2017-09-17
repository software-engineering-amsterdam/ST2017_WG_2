-- Started at 14:00, finished at 14:45

-- The more samples we take, the closer to normally distributed the resulted random list is
-- Examples:
-- n = 1000 -> [0.246,0.238,0.266,0.25]
-- n = 10000 -> [0.2525,0.2479,0.2467,0.2529]
-- n = 100000 -> [0.24855,0.24968,0.25241,0.24936]
-- n = 1000000 -> [0.249967,0.24968,0.250168,0.250185]
-- This shows the more samples, the closer the quartiles are to 0.25
-- In theory, if we take an infinite amount of samples, this would mean we would get [0.25,0.25,0.25,0.25]

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

-- Returns list of 4 ints that counted how many of n generated numbers using probs
-- are in (0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1) respectively.
quartiles :: [Float] -> [Int]
quartiles l = [q1, q2, q3, q4]
    where q1 = length (filter (\x -> x >= 0 && x <= 0.25) l)
          q2 = length (filter (\x -> x > 0.25 && x <= 0.5) l)
          q3 = length (filter (\x -> x > 0.5 && x <= 0.75) l)
          q4 = length (filter (\x -> x > 0.75 && x <= 1) l)

-- Normalize quartiles
normalizeQ :: Int -> [Int] -> [Float]
normalizeQ n l = map (\x -> fromIntegral(x) / fromIntegral(n)) l

ex1 :: Int -> IO [Float]
ex1 n = do
    p <- probs n
    return (normalizeQ n (quartiles p))

main = ex1 1000000