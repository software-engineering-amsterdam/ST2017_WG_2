-- Started at 16:00, finished at 16:15 (the programming part). Finished answering the questions at 16:30.

-- The number of list permutations is n! (n factorial)
-- This makes it even harder to test than the previous exercise, as the factorial gets very big even quicker.
-- Therefore on my PC, test cases with n >= 13 are very slow (12! > 6.000.000.000),
-- which would cause 100 test cases to take a really, really long time as a significant amount of test
-- cases use n >= 13.

module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

factorial :: Integer -> Integer
factorial n = if n < 2 then 1 else n * factorial(n-1)

workshopExerciseFive :: Integer -> Bool
workshopExerciseFive n = fromIntegral (length (permutations [1..n])) == factorial n

main = quickCheck (\ x -> (x >= 0) --> workshopExerciseFive x)