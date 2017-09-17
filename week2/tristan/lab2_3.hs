-- Result of ordering is ["ex3a","ex3c","ex3d","even","ex3b"]
-- 
-- This took ~50 minutes

module Lab2_3 (NamedProperty (NamedProperty, name, prop), sortStrength) where 

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data NamedProperty a = NamedProperty {name :: String, prop :: (a -> Bool)}
instance Show (NamedProperty a) where
   show (NamedProperty name _) = show name

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

ex3a :: Int -> Bool
ex3a x = even x && x > 3

ex3b :: Int -> Bool
ex3b x = even x || x > 3

ex3c :: Int -> Bool
ex3c x = (even x && x > 3) || even x

ex3d :: Int -> Bool 
ex3d x = (even x && x > 3) || even x

strongest :: [a] -> NamedProperty a -> NamedProperty a -> Ordering
strongest input (pre) (post)
    | prepost && postpre = EQ
    | prepost = GT
    | postpre = LT
    | otherwise = error ("Input fails for both")
    where prepost = stronger input (prop pre) (prop post)
          postpre = stronger input (prop post) (prop pre)

sortStrength :: [a] -> [NamedProperty a] -> [NamedProperty a]
sortStrength _ [] = []
sortStrength input properties = sortBy (flip (strongest input)) properties

main = do
    let range = [(-10)..10]
    
    let properties = [ NamedProperty {name = "ex3a", prop = ex3a}
                     , NamedProperty {name = "ex3b", prop = ex3b}
                     , NamedProperty {name = "ex3c", prop = ex3c}
                     , NamedProperty {name = "ex3d", prop = ex3d}
                     , NamedProperty {name = "even", prop = even} ]

    print (sortStrength range properties)
