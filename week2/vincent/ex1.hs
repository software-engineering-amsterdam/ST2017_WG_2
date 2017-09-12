-- ID: 11408227
-- Name: Vincent Jong
-- Time: 14:00 - 15:00 (Programming), (Answering)

import Data.List
import Data.Char
import System.Random

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1) 
            return (p:ps)

amountInQuartile :: [Float] -> Float -> Float -> Bool -> Int
amountInQuartile xs lb ub incl = length $ filter p xs
    where p x = if incl then (x >= lb && x <= ub) else (x > lb && x <= ub)

testProbs :: IO [Int]
testProbs = do
    list <- probs 10000
    let quartile1 = amountInQuartile list 0.0 0.25 True
    let quartile2 = amountInQuartile list 0.25 0.5 False
    let quartile3 = amountInQuartile list 0.5 0.75 False
    let quartile4 = amountInQuartile list 0.75 1.0 False
    return [quartile1, quartile2, quartile3, quartile4]

main = testProbs

