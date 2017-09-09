-- Started at 15:25, finished at 15:30 (the programming part). Finished answering the questions at 16:00.

-- It is hard to test, as the amount of subsequences doubles for each increment of the list size by 1.
-- Calculating the size of such a large list takes a long time.
-- Therefore on my PC, test cases with n >= 30 are very slow (2^30 > 1.000.000.000),
-- which would cause 100 test cases to take a really long time as a significant amount of test
-- cases use n >= 30.

-- To answer the question asking what we are testing actually:
-- I think we are indeed testing whether subsequences satisfies a part of its specification.
-- We proved by induction that if A is a finite set with |A|=n, then |P(A)|=2n.
-- As the subsequences function is a function to generate the Power Set of a list, we
-- are testing if the subsequences function generates a Power Set of the expected size.
-- We are not testing whether the the subsets of the generated
-- Power Set are correct, so we are indeed testing a part of the specification: the size of the generated list.

module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

workshopExerciseFour :: Int -> Bool
workshopExerciseFour n = length (subsequences [1..n]) == 2^n

main = quickCheck (\ x -> (x >= 0) --> workshopExerciseFour x)