-- ID: 11408227
-- Name: Vincent Jong
-- Time: 11:20 - 12:25 (Programming), (Testing), (Answering)

module Ex6 (trClos) where

import Data.List
import Test.QuickCheck
import System.Random
import SetOrd
import Data.Function

type Rel a = [(a, a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- From the book 
isTrans :: Ord a => Rel a -> Bool
isTrans [] = True
isTrans s = and [ trans pair s | pair <- s ] where
    trans (x,y) r = and [ elem (x,v) r | (u,v) <- r, u == y ]

makeTrans :: Ord a => Rel a -> Rel a
makeTrans rel = nub ((rel @@ rel) ++ rel)

sortRel :: Ord a => Rel a -> Rel a
sortRel rel = sortBy (compare `on` fst) (sortBy (compare `on` snd) rel)

trClos :: Ord a => Rel a -> Rel a
trClos rel = if isTrans rel 
    then sortRel rel
    else trClos (makeTrans rel)

testRel = [(1, 2), (2, 3), (3, 4)]

main = do
    trClos testRel