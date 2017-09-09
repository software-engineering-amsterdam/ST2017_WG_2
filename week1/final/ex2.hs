-- About 45min

-- Is the property hard to test? If you find that it is, can you given a reason why?
-- 		Yes, because finding subsequences of lists that are big is taking an impractical long time to test

-- Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually? 
-- Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification? 
-- Or are you testing something else still?
-- 		You are testing if lists with (pseudo-)random sizes have an amount of subsequences that equals 2^n where the size of the list equals n.
--		The fact that 'subsequences' satisfies the mathematical fact (that the length of subsequences should equal 2^n) for the limited amount of test cases does not mean that the implementation of subsequences is correct (as mentioned by Ana Oprescu during the lecture).

module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

workshopExerciseFour :: Int -> Bool
workshopExerciseFour n = length (subsequences [1..n]) == 2^n

main = quickCheck (\ x -> (x >= 0) --> workshopExerciseFour x)