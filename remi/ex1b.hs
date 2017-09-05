-- Started at 14:30, finished at 15:00

-- First, I just returned True if the input number by quickCheck was negative. This would mean that a lot of the test cases were basically discarded.
-- I thought a better solution would be to take the absolute value of the input number, so all test cases are actually used.
-- After asking questions about it during the lab, using the infix operator --> to check if the input is a natural number
-- when executing quickCheck is a good solution.

module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

workshopExerciseThree :: Integer -> Bool
workshopExerciseThree n = sum (map (^3) [1..n]) == (n*(n+1) `div` 2)^2

main = quickCheck (\ x -> (x >= 0) --> workshopExerciseThree x)