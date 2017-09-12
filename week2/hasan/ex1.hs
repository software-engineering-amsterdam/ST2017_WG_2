-- About 1 hour

-- Q: Implement this test, and report on the test results.
-- A: [0.250237,0.250332,0.249473,0.249958] are the percentages and as you can see the difference is quite small
-- It becomes even smaller and smaller and reaches 0.25 once n = infinity 


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

countInRange :: [Float] -> Float -> Float -> Int
countInRange n min max = length (filter (\x -> min <= x && x <= max) n)
             
countQuantiles :: [Float] -> [Float]
countQuantiles n = [
                        fromIntegral (countInRange n 0.00 0.25) / fromIntegral (length n),
                        fromIntegral (countInRange n 0.25 0.50) / fromIntegral (length n),
                        fromIntegral (countInRange n 0.50 0.75) / fromIntegral (length n),
                        fromIntegral (countInRange n 0.75 1.00) / fromIntegral (length n)
                    ]
main = do
    print "hallo"
    numbers <- probs 1000000
    print (countQuantiles (numbers))