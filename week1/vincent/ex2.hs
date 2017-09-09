-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~20 min

import Test.QuickCheck
import Data.List

setLengthPower :: Int -> Int
setLengthPower x = if x <= 1 then 1 else if x >= 20 then length (subsequences [1..20]) else length (subsequences [1..x])

setLengthPowerTheory :: Int -> Int
setLengthPowerTheory x = if x <= 1 then 1 else if x >= 20 then 2 ^ 20 else 2 ^ x

exer4 :: Int -> Bool
exer4 x = setLengthPower (x) == setLengthPowerTheory (x)

main = quickCheck exer4

-- This becomes harder to test the larger the random integer is. I am also simply checking for mathmetical equality between the
-- results of the functions and am not validating the result of the method 'subsequences'. I added a guard for if the random
-- integer is above 20