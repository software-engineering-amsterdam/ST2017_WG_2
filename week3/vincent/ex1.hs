-- ID: 11408227
-- Name: Vincent Jong
-- Time: 13:45 - 14:15 (Programming), (Testing), (Answering)

-- Testing by applying the functions to some well known statements

module Ex1 (contradiction, tautology, entails, equiv) where

import Data.List
import Test.QuickCheck
import System.Random
import Lecture3

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\r -> evl r f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = entails a b && entails b a

stmt1 = Cnj [p, (Neg p)]
stmt2 = Dsj [p, (Neg p)]

main = do
    print $ contradiction stmt1
    print $ tautology stmt2