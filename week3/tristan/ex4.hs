-- This took ~1.2 hour

module Ex4 where

import Control.Monad
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Ex1
import Ex3

instance Arbitrary Form where
    arbitrary = formGenerator 10

formGenerator :: Int -> Gen Form
formGenerator 0 = liftM Prop (choose (0, 10))
formGenerator depth = oneof [ liftM  Prop (choose (0, 10))
                            , liftM  Neg (formGenerator nextDepth)
                            , liftM  Cnj (vectorOf depth (formGenerator nextDepth))
                            , liftM  Dsj (vectorOf depth (formGenerator nextDepth))
                            , liftM2 Impl (formGenerator nextDepth) (formGenerator nextDepth)
                            , liftM2 Equiv (formGenerator nextDepth) (formGenerator nextDepth) ]
                    where nextDepth = depth - 1

prop_equiv, prop_names, prop_all :: Form -> Bool
prop_equiv form = equiv form (cnf form)
prop_names form = do
    let names1 = propNames form
    let names2 = propNames (cnf form)
    all (\ x -> elem x names1) names2
prop_all form = prop_equiv form && prop_names form

main = do
    quickCheck (forAll (formGenerator 6) prop_all)