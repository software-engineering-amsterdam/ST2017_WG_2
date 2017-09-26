-- About 10 minutes

module Ex5 where

import Data.List
import System.Random
import Test.QuickCheck

type Rel a = [(a,a)]

-- Add all inverses to the Rel
addInverses :: Ord a => Rel a -> Rel a
addInverses [] = []
addInverses ((a, b):c) = [(a, b), (b, a)] ++ addInverses c

-- Get the Rel with all its inverses added, then take the unique values
symClos :: Ord a => Rel a -> Rel a
symClos r = sort (nub (addInverses r))

main5 :: IO ()
main5 = do
    print (symClos [(1,2),(2,3),(3,4)])
    print (symClos [(0,1),(1,1),(1,2),(2,0),(2,2),(3,0)])