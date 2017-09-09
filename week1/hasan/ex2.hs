-- About 45min

-- Is the property hard to test? If you find that it is, can you given a reason why?
-- 		Yes, because finding subsequences of lists that are big is taking an impractical long time to test

-- Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually? 
-- Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification? 
-- Or are you testing something else still?
-- 		You are testing if lists with (pseudo-)random sizes have an amount of subsequences that equals 2^n where the size of the list equals n.
--		The fact that 'subsequences' satisfies the mathematical fact (that the length of subsequences should equal 2^n) for the limited amount of test cases does not mean that the implementation of subsequences is correct (as mentioned by Ana Oprescu during the lecture).

-- Import libraries
import Test.QuickCheck
import Data.List

-- Method
ex2 :: Int -> Int
ex2 n = length (subsequences [1..n])

-- Make a number positive
makePositive :: Int -> Int
makePositive n = if n <= 0 then 0 else if n > 20 then 10 else n

-- Test
ex2Test :: Int -> Bool
ex2Test n = ex2 n == 2^n

-- Only test numbers >= 0
ex2WrapperTest n = ex2Test (makePositive n)

-- Print test results
--main = print(ex2 5)
main = quickCheck ex2WrapperTest