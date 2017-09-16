-- About 3 hours
-- Q: You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?
-- A: That your tests do not have to contain cases in which the lists contain duplicates.
--
-- The automated test reports that property 1 to n is already sorted (because they all hold and therefore they are all equal)

module Lab2Ex4 where
import Data.List
import Data.Eq
import Test.QuickCheck
    
-- Implementation given by exercise week 2
infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- Exercise says we can assume no duplicates
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation a b = length (filter (\x -> not (elem x a)) b) == 0

--property1, property2, property3, property4, property5, property6 :: [Int] -> Bool
property1 a = isPermutation a a
property2 a = isPermutation a (sort a)
property3 a = isPermutation (sort a) a
property4 a = isPermutation (sort a) (sort a)
property5 a = isPermutation a (reverse a)
property6 a = isPermutation (reverse a) a
property7 a = isPermutation (reverse a) (reverse a)
property8 a = all (\x -> isPermutation a x) (subsequences a)
property9 a = all (\x -> isPermutation a x) (permutations a)

data PropertyWithName = PropertyWithName{ex :: ([Int] -> Bool), name :: String}
property1WithName = PropertyWithName{ex = property1, name = "property1"}
property2WithName = PropertyWithName{ex = property2, name = "property2"}
property3WithName = PropertyWithName{ex = property3, name = "property3"}
property4WithName = PropertyWithName{ex = property4, name = "property4"}
property5WithName = PropertyWithName{ex = property5, name = "property5"}
property6WithName = PropertyWithName{ex = property6, name = "property6"}
property7WithName = PropertyWithName{ex = property7, name = "property7"}
property8WithName = PropertyWithName{ex = property8, name = "property8"}
property9WithName = PropertyWithName{ex = property9, name = "property9"}

numbers = permutations [-2..3]
sortOnStrength a b 
            | (stronger numbers (ex a) (ex b) && weaker numbers (ex b) (ex a)) = GT
            | (stronger numbers (ex b) (ex a) && weaker numbers (ex a) (ex b)) = LT
            | otherwise = EQ

main = do
    let sortedAscending = sortBy sortOnStrength [property1WithName, property2WithName, property3WithName, property4WithName, property5WithName, property6WithName, property7WithName, property8WithName] 
    let sortedDescending = reverse sortedAscending
    print (map (\x -> name x) sortedDescending)