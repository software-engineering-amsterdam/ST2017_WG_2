-- Time: about 1 hour

module Ex1 where

import Data.List
import System.Random
import Lecture6

exM :: Integer -> Integer -> Integer -> Integer
exM x y n = if n == 1
    then 0
    else exMh x y n

exMh :: Integer -> Integer -> Integer -> Integer
exMh x y n | y == 0 = 1 `mod` n
            | y == 1 = x `mod` n
            | even y = inter * inter `mod` n
            | otherwise = (x * (exMh x (y-1) n)) `mod` n
                where inter = exMh x (y `div` 2) n
