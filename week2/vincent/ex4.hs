-- ID: 11408227
-- Name: Vincent Jong
-- Time: 10:15 - 10:40, 11:05 - 11:20 (Programming)

import Test.QuickCheck
import Data.List

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

permList = [1..5]

permList1 = [10, 25, 55, 37, 81, 43, 78]
permList2 = [55, 37, 43, 78, 81, 25, 10]
permList3 = [37, 42, 78, 25, 81, 55, 10]
permListClones1 = [25, 25, 55, 37, 43, 78, 10]
permListClones2 = [25, 55, 37, 43, 78, 10, 25]
permListClones3 = [25, 55, 55, 37, 43, 78, 10]

countElem :: Eq a => a -> [a] -> Int
countElem a [] = 0
countElem a (x:xs) = (if a == x then 1 else 0) + countElem a xs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (\x -> x `elem` ys) xs && all (\y -> y `elem` xs) ys 
-- to check for clones: && all (\x -> countElem x xs == countElem x ys) xs
-- alternative: xs `elem` permutations ys

factorial :: Int -> Int
factorial x = if x < 2 then 1 else x * factorial (x - 1)

checkSameList xs = isPermutation xs xs

checkSort xs = isPermutation xs (sort xs)

checkReverse xs = isPermutation xs (reverse xs)

checkNrPerms xs = factorial (length xs) == length (permutations xs)

--checkClones :: Bool
--checkClones = isPermutation per ys

--checkSameElements :: Bool
--checkClones xs ys = isPermutation


