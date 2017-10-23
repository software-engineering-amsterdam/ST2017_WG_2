-- Time: 14:45 - 15:30 (about 45 min)

module Ex3 where

import Data.List
import System.Random
import Lecture6

naturalnrs = [0..]

sqrt' :: Integer -> Integer
sqrt' = floor . sqrt . fromIntegral

-- divisible nrs from 2 to the square root of x
divNrs x = [2..(sqrt' x)]

isComposite :: Integer -> Bool
isComposite x = any (\y -> x `mod` y == 0) (divNrs x)

composites :: [Integer]
composites = [ x | x <- naturalnrs, isComposite x ]