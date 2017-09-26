-- About 1 hour

module Ex4 where

import Data.List
import System.Random
import Control.Monad
import Test.QuickCheck
import Lecture3
import Ex1
import Ex3


-- Random form generator inspired by:
-- https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/slides/meiser.pdf
-- https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
instance Arbitrary Form where
    arbitrary = sized arbForm

arbForm :: Int -> Gen Form
arbForm 0 = liftM Prop (choose (1, 3))
arbForm n = oneof [
                    liftM Prop (choose (1, 3)),
                    liftM Neg (arbForm (n `div` 2)),
                    liftM Cnj (vectorOf 2 (arbForm (n `div` 2))),
                    liftM Dsj (vectorOf 2 (arbForm (n `div` 2))),
                    liftM2 Impl (arbForm (n `div` 2)) (arbForm (n `div` 2)),
                    liftM2 Equiv (arbForm (n `div` 2)) (arbForm (n `div` 2))
                ]

-- Test if cnf is equivalent to original form
propEquiv :: Form -> Bool
propEquiv x = equiv (cnf x) x

-- Check if property names of the CNF are subset of the property names of the original form
checkPropNames :: Form -> Bool
checkPropNames f = all (\x -> elem x originalPropNames) (propNames (cnf f)) where originalPropNames = propNames f

main4 :: IO ()
main4 = do
    quickCheck propEquiv
    quickCheck checkPropNames