-- About 30min

-- Import libraries
import Test.QuickCheck

-- Method
ex1a :: Integer -> Integer
ex1a n = if n <= 1 then 1 else n*n + ex1a (n-1)

-- Make a number positive
makePositive :: Integer -> Integer
makePositive n = if n <= 0 then 1 else n

-- Test
ex1aTest :: Integer -> Bool
ex1aTest n = ex1a n == n*(n+1)*(2*n+1) `div` 6

-- Only test numbers >= 1
ex1aWrapperTest n = ex1aTest (makePositive n)

-- Print test results
--main = print(ex1a 5)
main = quickCheck ex1aWrapperTest