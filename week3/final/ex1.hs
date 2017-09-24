-- About 30 minutes
-- Definitions are checked using the lab exercises known to be correct

-- We all had a comparable solution, so we chose the one with most examples to check the functions

module Ex1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = entails f g && entails g f

main = do
    print (satisfiable (Dsj [p, Neg q]))
    print (contradiction (Cnj [p, Neg p]))
    print (tautology (Dsj [p, Neg p]))
    print (entails p (Dsj [p, q]))
    print (tautology (Impl p (Dsj [p, q])))
    print (equiv (Impl p q) (Impl (Neg q) (Neg p)))
    print (satisfiable (Impl (Dsj [p, q]) p))