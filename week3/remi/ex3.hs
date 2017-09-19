-- About 2.5 hours

module Ex3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Ex1

-- First transform form into arrow free form
-- Next, transform arrow free form into negation normal form
-- Finally, simplify
cnf :: Form -> Form
cnf f = simplify sf
    where af = arrowfree f
          nf = nnf af
          sf = distribute nf

-- Distribute ORs inwards over ANDs
distribute :: Form -> Form
distribute (Prop x) = Prop x
distribute (Dsj (a:b:c))
    | c /= [] = distribute (Dsj [distribute (Dsj [a, b]), distribute (Dsj c)])
distribute (Dsj [x, Cnj[y, z]]) = Cnj [distribute (Dsj [x, y]), distribute (Dsj [x, z])]
distribute (Dsj [Cnj[y, z], x]) = Cnj [distribute (Dsj [y, x]), distribute (Dsj [z, x])]
distribute (Dsj fs) = Dsj (map distribute fs)
distribute (Cnj fs) = Cnj (map distribute fs)
distribute x = x

-- Simplify tautology to +(true, false)
-- Simplify contradiction to *(true, false)
-- Simplify conjunction between equivalent expressions to one of the expressions
-- Simplify disjunction between equivalent expressions to one of the expressions
simplify :: Form -> Form
simplify (Prop x) = Prop x
simplify fs
   | tautology fs = Dsj [Prop propName, Neg (Prop propName)]
   | contradiction fs = Cnj [Prop propName, Neg (Prop propName)]
   where propName = head (propNames fs)
simplify (Cnj [x, y])
    | equiv x y = simplify x
    | tautology x = simplify y
    | tautology y = simplify x
    | contradiction x = simplify x
    | contradiction y = simplify y
    | otherwise = Cnj [simplify x, simplify y]
simplify (Dsj [x, y])
    | equiv x y = simplify x
    | tautology x = simplify y
    | tautology y = simplify x
    | contradiction x = simplify y
    | contradiction y = simplify x
    | otherwise = Dsj [simplify x, simplify y]
simplify (Dsj fs) = Dsj (map simplify fs)
simplify (Cnj fs) = Cnj (map simplify fs)
simplify x = x

main3 :: IO ()
main3 = do
    print (cnf (head (parse "-+(1 2)")))
    print (cnf (head (parse "+(1 *(1 2))")))
    print (cnf (head (parse "((1 ==> 2) <=> (-2 ==> -1))")))
    print (cnf (head (parse "(1 ==> -+(2 -3))")))
    print (cnf (head (parse "+(-(1 ==> 2) (3 ==> 1))")))
    print (cnf (head (parse "+(*(1 2) 3)")))
    print (cnf (head (parse "(1<=>-5)")))

    print (cnf (head (parse "*(8 4 (1<=>-5))")))
    print (cnf (head (parse "+(1 2 3 4)")))

    print (cnf (head (parse "+(*(1 -2) *(3 4) *(3 -4))")))

