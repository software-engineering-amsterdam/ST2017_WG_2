-- Running with 100000 numbers:   (0.2473,    0.25186,   0.25022,   0.25062)
-- Running with 1000000 numbers:  (0.249545,  0.250017,  0.250334,  0.250104)
-- Running with 10000000 numbers: (0.2501901, 0.2499938, 0.2498665, 0.2499496)
--
-- As the test results show that generating more random numbers results in a better distribution
-- We can see that 10000000 numbers approach the 0.25 distribution range, which would be expected
-- if the random number generator was perfect (for infinite case).
-- 
-- This took 1 hour

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

countQuartiles :: [Float] -> (Int, Int, Int, Int)
countQuartiles [] = (0, 0, 0, 0)
countQuartiles numbers = do
    let quartilized = map quartilize numbers
    foldl (addQuartiles) (0, 0, 0, 0) quartilized

quartileFrequency :: (Int, Int, Int, Int) -> (Float, Float, Float, Float)
quartileFrequency (qa, qb, qc, qd) = do
    let total = qa + qb + qc + qd
    (divideIntegral qa total, divideIntegral qb total, divideIntegral qc total, divideIntegral qd total)

divideIntegral :: Int -> Int -> Float
divideIntegral x y = fromIntegral (x) / fromIntegral (y)

quartilize :: Float -> (Int, Int, Int, Int)
quartilize number
    | number <= 0.25 = (1, 0, 0, 0)
    | number <= 0.5  = (0, 1, 0, 0) 
    | number <= 0.75 = (0, 0, 1, 0)
    | otherwise      = (0, 0, 0, 1)

addQuartiles :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addQuartiles (a, b, c, d) (e, f, g, h) = (a+e, b+f, c+g, d+h)

main = do
    numbers <- probs 10000000
    print (quartileFrequency (countQuartiles numbers))