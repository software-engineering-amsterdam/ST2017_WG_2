-- ID: 11408227
-- Name: Vincent Jong
-- Time: 14:15 - 14:30 (Programming), (Testing), (Answering)

-- SymClos . TransClos of a relation is not the same as TransClos . SymClos. 

module Ex8 where

import Data.List
import Test.QuickCheck
import System.Random
import SetOrd
import Ex3
import Ex5
import Ex6
import Ex7

type Rel a = [(a, a)]

main = do
    rel1 <- genRel 5
    print rel1
    let relm1 = trClos (symClos rel1)
    let relm2 = symClos (trClos rel1)
    print relm1
    print relm2
    print $ relm1 == relm2