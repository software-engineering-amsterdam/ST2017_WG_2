-- This took ~15 minutes

import Debug.Trace
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lab2_4

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = False
isDerangement x y = isPermutation x y && all (\(a, b) -> a /= b) (zip x y)

deran :: Int -> [[Int]]
deran n = do
    let range = [0..n-1]
    filter (isDerangement range) (permutations range)

prop_reversed, prop_derangement :: (Eq a, Ord a) => [a] -> Bool
prop_reversed [] = not (isDerangement ([] :: [Int]) ([] :: [Int]))
prop_reversed [x] = not (isDerangement [x] [x])
prop_reversed x = do
    let sorted = sort x
    isDerangement sorted ((tail x) ++ [head x])

prop_derangement x = prop_reversed x

main = do
    print (isDerangement [1, 2] [2, 1] == True)
    print (isDerangement [1, 2, 3] [2, 3, 1] == True)
    print (isDerangement ([] :: [Int]) ([] :: [Int]) == False)
    print (isDerangement [1] [1] == False)
    print (isDerangement [2, 1] [1] == False)
    print (isDerangement [2, 1] [1] == False)

    -- quickCheck (forAll generatePositiveInt (\x -> all (prop_derangement [0..x-1]) (deran x)))
    quickCheck (prop_derangement :: [Int] -> Bool)