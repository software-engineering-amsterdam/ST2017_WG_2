-- About 15 minutes
-- We chose this solution because it the best implementation of our group.

-- The transitive closure is called recursively until the resulting relation is transitive.
-- If the resulting relation is transitive, it is sorted and returned.
-- To check if a relation is transitive, we use the transR function from the Haskell Road book.

module Ex6 where

import Data.List
import System.Random
import Test.QuickCheck
import Ex5

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

-- Check if relation is transitive
-- Inspired by the book (Haskell Road)
transR :: Ord a => Rel a -> Bool
transR [] = True
transR s = and [trans pair s | pair <- s]
    where trans (x,y) r = and [(x,v) `elem` r | (u,v) <- r, u == y]

-- S = R ∪ R2 ∪ ··· ∪ Rk
-- Until S is transitive
trClos :: Ord a => Rel a -> Rel a
trClos r
    | transR r = sort r
    | otherwise = trClos rs
    where rs = nub (r ++ (r @@ r))

main6 :: IO ()
main6 = do
    print (trClos [(1,2),(2,3),(3,4)])
    print (trClos [(0,2), (0,3), (1,0), (1,3), (2,0), (2,3)])