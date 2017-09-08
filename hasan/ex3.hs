-- About 45min

-- Is the property hard to test? If you find that it is, can you given a reason why?
-- 		It is indeed hard to test because finding permutations of large lists is timeconsuming.

-- Again, give your thoughts on the following issue: when you perform the test for exercise 5, what are you testing actually? 
-- Are you checking a mathematical fact? Or are you testing whether perms satisfies a part of its specification? 
-- Or are you testing something else still?
-- 		You are testing if lists with (pseudo-)random sizes have an amount of permutations that equals n! where the size of the list equals n.
--		The fact that 'permutations' satisfies the mathematical fact (that the length of subsequences should equal n!) for the limited amount of test cases does not mean that the implementation of permutations is correct (as mentioned by Ana Oprescu during the lecture).

-- Import libraries
import Test.QuickCheck
import Data.List

-- Method
ex3 :: Int -> Int
ex3 n = if n <= 1 then 1 else n * ex3 (n-1)

-- Make a number positive
makePositive :: Int -> Int
makePositive n = if n <= 0 then 0 else if n > 10 then 10 else n

-- Test
ex3Test :: Int -> Bool
ex3Test n = ex3 n == length (permutations [1..n])

-- Only test numbers >= 0
ex3WrapperTest n = ex3Test (makePositive n)

-- Print test results
--main = print(ex3 5)
main = quickCheck ex3WrapperTest