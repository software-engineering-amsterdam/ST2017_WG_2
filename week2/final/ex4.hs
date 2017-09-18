-- About 3 hours
-- Q: You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?
-- A: That your tests do not have to contain cases in which the lists contain duplicates, so we can test using ranges.
--
-- The automated ordering of property strength reports that
-- property 1 to n is already sorted (because they all hold and therefore they are all equal)

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

-- isPermutation implementation
deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst x (b:bc)
    | x == b = bc
    | otherwise = b : deleteFirst x bc

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] b = b == []
isPermutation a b
    | length a /= length b = False
    | otherwise = isPermutation xs (deleteFirst x b)
    where x = head a
          xs = tail a

--property1, property2, property3, property4, property5, property6 :: [Int] -> Bool
property1 a = isPermutation a a
property2 a = isPermutation a (sort a)
property3 a = isPermutation (sort a) a
property4 a = isPermutation (sort a) (sort a)
property5 a = isPermutation a (reverse a)
property6 a = isPermutation (reverse a) a
property7 a = isPermutation (reverse a) (reverse a)

data PropertyWithName = PropertyWithName{ex :: ([Int] -> Bool), name :: String}
property1WithName = PropertyWithName{ex = property1, name = "property1"}
property2WithName = PropertyWithName{ex = property2, name = "property2"}
property3WithName = PropertyWithName{ex = property3, name = "property3"}
property4WithName = PropertyWithName{ex = property4, name = "property4"}
property5WithName = PropertyWithName{ex = property5, name = "property5"}
property6WithName = PropertyWithName{ex = property6, name = "property6"}
property7WithName = PropertyWithName{ex = property7, name = "property7"}

numbers = permutations [-2..3]
sortOnStrength a b
            | (stronger numbers (ex a) (ex b) && weaker numbers (ex b) (ex a)) = GT
            | (stronger numbers (ex b) (ex a) && weaker numbers (ex a) (ex b)) = LT
            | otherwise = EQ

-- Test if input meets all above properties
testProperties :: [Int] -> Bool
testProperties a = property1 a && property2 a && property3 a && property4 a && property5 a && property6 a && property7 a

-- Hoare test function for the properties of the isPermutation function
hoareTest :: (a -> Bool) -> (a -> Bool) -> (Bool -> Bool) -> [a] -> Bool
hoareTest precondition f postcondition =
    all (\x -> precondition x --> postcondition (f x))

main = do
    let sortedAscending = sortBy sortOnStrength [property1WithName, property2WithName, property3WithName, property4WithName, property5WithName, property6WithName, property7WithName]
    let sortedDescending = reverse sortedAscending
    print (map (\x -> name x) sortedDescending)

    -- Hoare test with lists of increasing range
    print (hoareTest (\_ -> True) testProperties (\x -> x) [[0..n] | n <- [0..99]])

    -- Quickcheck with randomly generated lists
    quickCheck testProperties