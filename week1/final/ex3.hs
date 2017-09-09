-- About 45min

-- Is the property hard to test? If you find that it is, can you given a reason why?
-- 		It is indeed hard to test because finding permutations of large lists is timeconsuming.

-- Again, give your thoughts on the following issue: when you perform the test for exercise 5, what are you testing actually? 
-- Are you checking a mathematical fact? Or are you testing whether perms satisfies a part of its specification? 
-- Or are you testing something else still?
-- 		You are testing if lists with (pseudo-)random sizes have an amount of permutations that equals n! where the size of the list equals n.
--		The fact that 'permutations' satisfies the mathematical fact (that the length of subsequences should equal n!) for the limited amount of test cases does not mean that the implementation of permutations is correct (as mentioned by Ana Oprescu during the lecture).

module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

factorial :: Integer -> Integer
factorial n = if n < 2 then 1 else n * factorial(n-1)

workshopExerciseFive :: Integer -> Bool
workshopExerciseFive n = genericLength (permutations [1..n]) == factorial n

main = quickCheck (\ x -> (x >= 0) --> workshopExerciseFive x)