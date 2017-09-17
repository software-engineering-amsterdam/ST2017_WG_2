module Lab2 where

import Data.List

data ExerciseWithName = ExerciseWithName{ex :: (Int -> Bool), name :: String}
ex3_1, ex3_2, ex3_3 :: Int -> Bool
ex3_1 x = even x && x > 3
ex3_2 x = even x || x > 3
ex3_3 x = (even x && x > 3) || even x
ex3_4 x = even x
ex3_1WithName = ExerciseWithName{ex = ex3_1, name = "even x && x > 3"}
ex3_2WithName = ExerciseWithName{ex = ex3_2, name = "even x || x > 3"}
ex3_3WithName = ExerciseWithName{ex = ex3_3, name = "(even x && x > 3) || even x"}
ex3_4WithName = ExerciseWithName{ex = ex3_4, name = "even x"}

-- Implementation given by exercise week 2
infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- Ordering function that orders based on strength of properties 
numbers = [-10..10]
sortOnStrength a b 
            | (stronger numbers (ex a) (ex b) && weaker numbers (ex b) (ex a)) = GT
            | (stronger numbers (ex b) (ex a) && weaker numbers (ex a) (ex b)) = LT
            | otherwise = EQ

main = do
    let sortedAscending = sortBy sortOnStrength [ex3_1WithName, ex3_2WithName, ex3_3WithName, ex3_4WithName] 
    let sortedDescending = reverse sortedAscending
    print (map (\x -> name x) sortedDescending)