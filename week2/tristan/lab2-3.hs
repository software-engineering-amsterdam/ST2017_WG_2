-- Result of ordering is ["ex3b","ex3c","ex3d","even","ex3a"]
-- 
-- This took ~50 minutes

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

data WrappedProperty a = WrappedProperty {name :: String, prop :: (a -> Bool)}
instance Show (WrappedProperty a) where
   show (WrappedProperty name _) = show name

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

strongest :: [a] -> WrappedProperty a -> WrappedProperty a -> Ordering
strongest input (pre) (post)
    | prepost && postpre = EQ
    | prepost = GT
    | postpre = LT
    | otherwise = error ("Input fails for both")
    where prepost = stronger input (prop pre) (prop post)
          postpre = stronger input (prop post) (prop pre)

sortStrength :: [a] -> [WrappedProperty a] -> [WrappedProperty a]
sortStrength _ [] = []
sortStrength input properties = sortBy (strongest input) properties

main = do
    let range = [(-10)..10]
    
    let properties = [ WrappedProperty {name = "ex3a", prop = ex3a}
                     , WrappedProperty {name = "ex3b", prop = ex3b}
                     , WrappedProperty {name = "ex3c", prop = ex3c}
                     , WrappedProperty {name = "ex3d", prop = ex3d}
                     , WrappedProperty {name = "even", prop = even} ]

    print (sortStrength range properties)
