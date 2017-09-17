-- ID: 11408227
-- Name: Vincent Jong
-- Time: 10:15 - 10:40, 11:05 - 11:20 (Programming)

-- Because we can assume that the input does not contain duplicates, we can leave out these
-- cases during testing. 

import Test.QuickCheck
import Data.List
import Data.Function

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

permList = permutations [1..5]

countElem :: Eq a => a -> [a] -> Int
countElem a [] = 0
countElem a (x:xs) = (if a == x then 1 else 0) + countElem a xs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (\x -> x `elem` ys) xs && all (\y -> y `elem` xs) ys 
-- to check for clones: && all (\x -> countElem x xs == countElem x ys) xs
-- alternative: xs `elem` permutations ys

factorial :: Int -> Int
factorial x = if x < 2 then 1 else x * factorial (x - 1)

-- Properties

checkSameList xs = isPermutation xs xs

checkSort xs = isPermutation xs (sort xs)

checkReverse xs = isPermutation xs (reverse xs)

checkNrPerms xs = factorial (length xs) == length (permutations xs)

-- Strength of properties

strengthProps :: ([Int] -> Bool) -> ([Int] -> Bool) -> Int
strengthProps x y = if stronger permList x y then 1 else 0

strengthProp1 = strengthProps checkSameList checkSort + 
                strengthProps checkSameList checkReverse +
                strengthProps checkSameList checkNrPerms

strengthProp2 = strengthProps checkSort checkSameList + 
                strengthProps checkSort checkReverse +
                strengthProps checkSort checkNrPerms

strengthProp3 = strengthProps checkReverse checkSameList + 
                strengthProps checkReverse checkSort +
                strengthProps checkReverse checkNrPerms

strengthProp4 = strengthProps checkNrPerms checkSameList + 
                strengthProps checkNrPerms checkReverse +
                strengthProps checkNrPerms checkSort

propStrengthList = [(strengthProp1, "sameList"), (strengthProp2, "sort"), (strengthProp3, "reverse"), (strengthProp4, "nrPerms")]

propStrengthListSorted :: [(Int, String)]
propStrengthListSorted = sortBy (flip compare `on` fst) propStrengthList

main = print propStrengthListSorted