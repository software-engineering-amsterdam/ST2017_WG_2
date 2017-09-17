-- Start 11:15

module Lab2Ex5 where
import Data.List

-- Implementation given by exercise week 2
infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- From ex4
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation a b = length (filter (\x -> not (elem x a)) b) == 0

isDerangement, isDerangementHelper :: [Int] -> [Int] -> Bool
isDerangement a b = length a > 1 && isPermutation a b && isDerangementHelper a b
isDerangementHelper [] [] = True
isDerangementHelper a b = (head a) /= (head b) && isDerangementHelper (tail a) (tail b)

deran :: [Int] -> [Int]
deran n = head (filter (\x -> isDerangement n x) (permutations n))

property1 :: [Int] -> Bool
property1 n = not (isDerangement n n)
property2 n = not (isDerangement sorted sorted) where sorted = (sort n)
property3 n = isDerangement sorted ((tail sorted) ++ [head sorted]) where sorted = (sort n)
property4 n = (isDerangement n n) == (isDerangement reversed reversed) where reversed = reverse n 

data PropertyWithName = PropertyWithName{ex :: ([Int] -> Bool), name :: String}
property1WithName = PropertyWithName{ex = property1, name = "property1"}
property2WithName = PropertyWithName{ex = property2, name = "property2"}
property3WithName = PropertyWithName{ex = property3, name = "property3"}
property4WithName = PropertyWithName{ex = property4, name = "property4"}

numbers = permutations [-2..3]
sortOnStrength a b 
            | (stronger numbers (ex a) (ex b) && weaker numbers (ex b) (ex a)) = GT
            | (stronger numbers (ex b) (ex a) && weaker numbers (ex a) (ex b)) = LT
            | otherwise = EQ

main = do
    let sortedAscending = sortBy sortOnStrength [property1WithName, property2WithName, property3WithName, property4WithName] 
    let sortedDescending = reverse sortedAscending
    print (map (\x -> name x) sortedDescending)

--main = do
--	print (isDerangement [1, 2] [2, 1])
--	print (isDerangement [1, 2, 3, 4] [4, 3, 2, 1])
--	print (isDerangement [1, 2, 3, 4] [4, 2, 3, 1])
--	print (deran [1..3])
--	putStrLn "\n\nproperties"
--	print (property1 [1, 3, 2, 4])
--	print (property2 [1, 3, 2, 4])
--	print (property3 [1, 3, 2, 4])
--	print (property3 [])
--	print (property3 [1])
--	print (property4 [1, 3, 2, 4])