-- ID: 11408227
-- Name: Vincent Jong
-- Time: 12:25 - (Programming), (Testing), (Answering)

module Ex7 where

import Data.List
import Test.QuickCheck
import System.Random
import SetOrd
import Ex5
import Ex6

type Rel a = [(a, a)]

instance Arbitrary (Rel a) where
    arbitrary = return [(1, 2), (2, 3), (3, 4)]