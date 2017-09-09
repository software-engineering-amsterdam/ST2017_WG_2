-- Answer to Lab1, question 3: is the property hard to test?
-- It depends on the definition of hard to test...
-- Writing the test was easy and took at most 5 minutes. So no, in that case this property is not hard to test.
-- When running, quickTest can generate huge integers which generate extremely large set of permutations
-- (assuming number of permutations of n items == n!, we see that large n generates insane sets).
-- In the large integer case, this property is hard to test, since calculating this property for large n is not feasable.
--
-- On whether we are testing a mathematical fact or the implementation of `permutations`:
-- since we can not possibly ever test the property with every natural number, we can never be sure
-- that subsequences is correct for every natural number. The only thing that we are testing is whether the mathimatical fact holds
-- for some random natural number using this implementation of generating permutations.
--
-- This took ~20 minutes

import Data.List
import Test.QuickCheck

-- For cases where natural numbers are needed (positive integers) the following
-- generator is used
generatePositiveInt :: Gen Int
generatePositiveInt = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

permutationsLength :: Int -> Int
permutationsLength n = product [1..n]

prop_permutationsLength :: Int -> Bool
prop_permutationsLength n = length (perms [1..n]) == permutationsLength n

main = quickCheck (forAll generatePositiveInt prop_permutationsLength)