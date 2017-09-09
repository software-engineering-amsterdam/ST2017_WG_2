-- About 45min

-- Import libraries
import Test.QuickCheck

-- Method
ex1b :: Integer -> Integer
ex1b n = if n == 1 then 1 else n*n*n + ex1b (n-1)

-- Make a number positive
makePositive :: Integer -> Integer
makePositive n = if n <= 0 then 1 else n

-- Test
ex1bTest :: Integer -> Bool
ex1bTest n = ex1b n == (n*(n+1) `div` 2)^2

-- Only test numbers >= 1
ex1bWrapperTest n = ex1bTest (makePositive n)

-- Print test results
--main = print(ex1b 5)
main = quickCheck ex1bWrapperTest