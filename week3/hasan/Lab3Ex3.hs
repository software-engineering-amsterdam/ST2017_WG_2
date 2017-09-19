-- About 

module Lab3Ex3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3Ex1

--containsCnj :: Form -> Bool

-- If a form is a tautology or contradiction, replace it with the simplest tautology or contradiction respectiveally
-- Replace single conjunctions and disjunctions such as *(1) and +(1) to 1
-- We don't have to match impl and equiv because we use this function after 'arrowfree'
cnfHelper :: Form -> Form
cnfHelper forms
    | tautology forms             = Dsj [Prop propName, Neg (Prop propName)] 
    | contradiction forms         = Cnj [Prop propName, Neg (Prop propName)]
                                    where propName = head (propNames forms)
cnfHelper (Neg form)               = Neg (cnf form)
cnfHelper (Cnj [Prop p])        = Prop p
cnfHelper (Dsj [Prop p])        = Prop p
cnfHelper (Cnj forms)            = Cnj (map cnf (nub forms))
cnfHelper (Dsj forms)            = Dsj (map cnf (nub forms))
cnfHelper forms = forms

cnf :: Form -> Form
cnf f = cnfHelper (nnf (arrowfree f))

mainLab3Ex3 = do
    let form_a = Equiv (Impl p q) (Impl (Neg q) (Neg p))
    let form_b = head (parse "+(*(1 2) 3)")
    --let form_b = head (parse "+(*(1 2 3 4 5) 6 7 8)")
    putStrLn "\nForm: "
    print (form_b)
    
    putStrLn "\nCNF conversion:"
    print (cnf form_b)