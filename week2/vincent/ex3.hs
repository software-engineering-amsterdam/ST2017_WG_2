-- ID: 11408227
-- Name: Vincent Jong
-- Time: 16:05 - 16:20, 10:40 - 11:05 (Programming), (Answering)

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

range = [(-10)..10]

-- Function ID: 0
propEven :: Int -> Bool
propEven x = x `mod` 2 == 0

-- Function ID: 1
propEvenAndGrThree :: Int -> Bool
propEvenAndGrThree x = propEven x && x > 3

-- Function ID: 2
propEvenOrGrThree :: Int -> Bool
propEvenOrGrThree x = propEven x || x > 3

-- Function ID: 3
propEvenAndGrThreeOrEven :: Int -> Bool
propEvenAndGrThreeOrEven x = propEvenAndGrThree x || propEven x

-- Expected order = [1,0,3,2] or [1,3,0,2]. 0 and 3 are interchangeable since they are stronger than each other.

strengthProps :: (Int -> Bool) -> (Int -> Bool) -> Int
strengthProps x y = if stronger range x y then 1 else 0

strengthPropEven :: Int
strengthPropEven = strengthProps propEven propEvenAndGrThree + strengthProps propEven propEvenOrGrThree + strengthProps propEven propEvenAndGrThreeOrEven

strengthPropEvenAndGrThree :: Int
strengthPropEvenAndGrThree = strengthProps propEvenAndGrThree propEven + strengthProps propEvenAndGrThree propEvenOrGrThree + strengthProps propEvenAndGrThree propEvenAndGrThreeOrEven

strengthPropEvenOrGrThree :: Int
strengthPropEvenOrGrThree = strengthProps propEvenOrGrThree propEven + strengthProps propEvenOrGrThree propEvenAndGrThree + strengthProps propEvenOrGrThree propEvenAndGrThreeOrEven

strengthPropEvenAndGrThreeOrEven :: Int
strengthPropEvenAndGrThreeOrEven = strengthProps propEvenAndGrThreeOrEven propEven + strengthProps propEvenAndGrThreeOrEven propEvenAndGrThree + strengthProps propEvenAndGrThreeOrEven propEvenOrGrThree

propStrengthList = [(strengthPropEven, "Even"), (strengthPropEvenAndGrThree, "EvenAndGr3"), (strengthPropEvenOrGrThree, "EvenOrGr3"), (strengthPropEvenAndGrThreeOrEven, "EvenAndGr3OrEven")]

propStrengthListSorted :: [(Int, String)]
propStrengthListSorted = sortBy (flip compare `on` fst) propStrengthList

main = print propStrengthListSorted
