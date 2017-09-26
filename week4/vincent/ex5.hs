-- ID: 11408227
-- Name: Vincent Jong
-- Time: 11:15 - 11:20 (Programming), (Testing), (Answering)

module Ex5 (symClos) where

import Data.List
import Test.QuickCheck
import System.Random
import SetOrd

type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos (rel:rels) = [rel, (snd rel, fst rel)] ++ symClos rels

main = do
    let testRel = [(1, 2), (2, 3), (3, 4)]
    symClos testRel