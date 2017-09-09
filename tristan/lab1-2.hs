-- Answer to Lab1, question 2: is the property hard to test?
-- It depends on the definition of hard to test...
-- Writing the test was easy and took at most 5 minutes. So no, in that case this property is not hard to test.
-- When running, quickTest can generate huge integers which generate extremely large set of subsequences
-- (assuming |P(A)| == 2^n, we see that large n generates insane sets).
-- In the large integer case, this property is hard to test, since calculating this property for large n is not feasable.
--
-- On whether we are testing a mathematical fact or the implementation of `subsequences`:
-- since we can not possibly ever test the property with every natural number, we can never be sure
-- that subsequences is correct for every natural number. The only thing that we are testing is whether |P(A)| == 2^n holds
-- for some random natural number using this implementation of generating a power set.
--
-- This took ~40 minutes

import Data.List
import Test.QuickCheck

-- For cases where natural numbers are needed (positive integers) the following
-- generator is used
generatePositiveInt :: Gen Int
generatePositiveInt = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

subsequencesLength :: Int -> Int
subsequencesLength n = 2^n 

prop_subsequencesLength :: Int -> Bool
prop_subsequencesLength n = length (subsequences [1..n]) == subsequencesLength n

main = quickCheck (forAll generatePositiveInt prop_subsequencesLength)