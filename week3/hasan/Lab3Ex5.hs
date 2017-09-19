module Lab3Ex5 where

import Lab3Ex3
import Lecture3
import Data.List

type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg x) = map (\y -> map (\z -> -z) y) (cnf2cls x)
cnf2cls (Dsj x) = [concat (concat (map cnf2cls x))]
cnf2cls (Cnj x) = concat (map cnf2cls x)
cnf2cls x = [[-1, -1, -1]]

mainLab3Ex5 = do
    let test1Form = head (parse "*(5 -6)")
    let test2Form = head (parse "+(5 -6)")
    let test3Form = head (parse "*(8 4 (1<=>-5))")
    let test3_cnfForm = head (parse "*(+(-1 1) +(5 1) +(-1 -5) +(5 -5) 8 4)")
    let normalForm = head (parse "*(4 +(5 -6))")
    let cnfForm = cnf normalForm
    let aaaa = head (parse "+(1 2 3 4)")
    let bbbb = head (parse "*(8 4 *(+(-5 -1) +(1 5)))")
    putStrLn "\n\nCNF:"
    print cnfForm
    putStrLn "\n\n"
    let clsForm = cnf2cls cnfForm
    print clsForm
    putStrLn "\n\n"
    print (cnf2cls (cnf bbbb))
    --print (cnf test3Form)
    --print (cnf2cls test1Form) 
    --print (cnf2cls test2Form) 
    --print (cnf2cls normalForm) 
    --print (cnf2cls (dsjTransform (cnf test3Form)))
    --print (dsjTransform (cnf test3Form))
    --print (cnf2cls test3_cnfForm) 