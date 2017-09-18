-- About 30min

module Lab3Ex1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- | logical entailment 
entails :: Form -> Form -> Bool
--entails f1 f2 = all (\ v -> evl v f1 == evl v f2) (allVals (Dsj [f1, f2]))
entails f1 f2 = tautology (Impl f1 f2)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = (entails f1 f2) && (entails f2 f1)

main = do
    let form_a = Equiv (Impl p q) (Impl (Neg q) (Neg p))
    let form_b = Cnj [p, (Neg p)]
    let form_c = Dsj [p, p]
    let form_d = Dsj [p, (Neg p)]
    let form_e = Equiv (Impl p q) (Impl (Neg q) (Neg p))
    let form_f = Dsj [p, (Neg p)]
    
    putStrLn "\nExample printing form:"
    print (form_a)
    
    putStrLn "\nContradiction:"
    print form_b
    print (contradiction form_b)
    
    putStrLn "\nTautology:"
    print (tautology form_c)
    print (tautology form_d)
    
    putStrLn "\nEntails:"
    print (entails form_e form_f)
    
    putStrLn "\nEquiv:"
    print (equiv form_e form_f)