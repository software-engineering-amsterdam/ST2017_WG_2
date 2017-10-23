-- About 30 minutes

module Ex1 where

import Data.List
import System.Random

exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 _ = 1
exM x y z
    | y `mod` 2 == 0 = m*m `mod` z
    | otherwise = x*m*m `mod` z
    where m = Ex1.exM x (y `div` 2) z

main = exM 3 32 5
