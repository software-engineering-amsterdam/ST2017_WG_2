-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~30 min

import Test.QuickCheck
import Data.List

-- Helper functions

factorial :: Int -> Int
factorial x = if x < 2 then 1 else x * factorial (x - 1)


setLengthPerm :: Int -> Int
setLengthPerm x = if x <= 1 then 1 else if x >= 20 then length (permutations [1..5]) else length (permutations [1..x])

setLengthPermTheory :: Int -> Int
setLengthPermTheory x = if x <= 1 then 1 else if x >= 20 then factorial (5) else factorial (x)

exer5 :: Int -> Bool
exer5 x = setLengthPerm (x) == setLengthPermTheory (x)

main = quickCheck exer5

-- This becomes harder to test the larger the random integer is. I am also simply checking for mathmetical equality between the
-- results of the functions and am not validating the result of the method 'permutations'. I added a guard for if the random
-- integer is above 20