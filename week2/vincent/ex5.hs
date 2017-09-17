-- ID: 11408227
-- Name: Vincent Jong
-- Time: 11:20 - 12:30, 14:30 - 15:45 (Programming), (Answering)

import Test.QuickCheck
import Data.List

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (\x -> x `elem` ys) xs && all (\y -> y `elem` xs) ys 

isAllDifPos :: [Int] -> [Int] -> Bool
isAllDifPos [] [] = True
isAllDifPos (x:xs) (y:ys) = x /= y && isAllDifPos xs ys

isDerangement :: [Int] -> [Int] -> Bool
isDerangement [] [] = False
isDerangement xs ys = length xs >= 2 && length ys >= 2 && isPermutation xs ys && isAllDifPos xs ys

deran :: [Int] -> [[Int]]
deran xs = [ y | y <- permutations xs, isDerangement xs y]

-- Properties

-- Using quickcheck also generates lists with duplicates..
propSortedReverse :: [Int] -> Bool
propSortedReverse xs = if xs == sort xs && length xs >= 2 then isDerangement xs (reverse xs) else True

propSameList :: [Int] -> Bool
propSameList xs = not (isDerangement xs xs)

propSymmetric :: [Int] -> [Int] -> Bool
propSymmetric xs ys = isDerangement xs ys --> isDerangement ys xs

main = do
    quickCheck propSortedReverse
    quickCheck propSameList
    quickCheck propSymmetric