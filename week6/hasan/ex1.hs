module Ex1 where

import Criterion.Main
import Lecture2
import Lecture6

-- expM x y = rem (x^y)
exMCustom :: Integer -> Integer -> Integer -> Integer
exMCustom a b c = 
    if b == 0 then 1 `mod` c
    else if b == 1 then a `mod` c
    else if b `mod` 2 == 0 then let x = (exMCustom a (b `div` 2) c) in (x*x) `mod` c
    else (a * (exMCustom a (b-1) c)) `mod` c

runExp1 :: [(Integer, Integer, Integer)] -> Bool
runExp1 triplets = True where x = map (\(a,b,c) -> (exMCustom a b c)) triplets
runExp2 :: [(Integer, Integer, Integer)] -> Bool
runExp2 triplets = True where x = map (\(a,b,c) -> (expM a b c)) triplets

generateRandomTriplets :: Int -> IO [(Integer, Integer, Integer)]
generateRandomTriplets 0 = return []
generateRandomTriplets n = do
    a <- getRandomInt 10000
    b <- getRandomInt 10000
    c <- getRandomInt 10000
    y <- generateRandomTriplets (n-1)
    return ((fromIntegral a, fromIntegral b, fromIntegral c):y)

main = do
    triplets <- generateRandomTriplets 100000
    defaultMain [
        bgroup "exptest" [ 
            bench "expM"  $ nf runExp1 triplets, 
            bench "expCustom"  $ nf runExp2 triplets
            ]
        ]