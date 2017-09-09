-- This took ~1 hour

import Test.QuickCheck

-- For cases where natural numbers are needed (positive integers) the following
-- generator is used
generatePositiveInt :: Gen Int
generatePositiveInt = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

powerSumNaive :: Int -> Int -> Int
powerSumNaive n p = foldl (\x y -> x + y^p) 0 [1..n]

power2Sum :: Int -> Int
power2Sum n = div (n * (n + 1) * (2 * n + 1)) 6

power3Sum :: Int -> Int
power3Sum n = div (n * (n + 1)) 2 ^ 2

prop_power2Sum :: Int -> Bool
prop_power2Sum n = power2Sum n == powerSumNaive n 2

prop_power3Sum :: Int -> Bool
prop_power3Sum n = power3Sum n == powerSumNaive n 3

main = do
    quickCheck (forAll generatePositiveInt prop_power2Sum)
    quickCheck (forAll generatePositiveInt prop_power3Sum)