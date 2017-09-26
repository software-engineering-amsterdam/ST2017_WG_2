-- About 3 hours

-- First, a form is transformed into arrow free form (code from lecture)
-- Next, transform arrow free form into negation normal form (code from lecture)
-- Next, ORs are distributed inwards over ANDs

-- Finally, the form is simplified:
-- Simplify tautology to +(true, false)
-- Simplify contradiction to *(true, false)
-- Simplify conjunction between a tautology or contradiction
-- Simplify disjunction between a tautology or contradiction
-- Simplify conjunction between equivalent expressions to one of the expressions
-- Simplify disjunction between equivalent expressions to one of the expressions

-- We chose this solution because we shared our ideas while implementing,
-- so we all contributed to it. This one turned out to be the most complete.

module Ex3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Ex1

isDsj :: Form -> Bool
isDsj (Dsj x) = True
isDsj x = False

cnf :: Form -> Form
cnf f = sf
    where af = arrowfree f
          nf = nnf af
          df = distribute nf
          sf = simplify df

distribute :: Form -> Form
distribute (Prop x) = Prop x
distribute (Dsj [x, Cnj[y, z]]) = Cnj [distribute (Dsj [x, y]), distribute (Dsj [x, z])]
distribute (Dsj [Cnj[y, z], x]) = Cnj [distribute (Dsj [y, x]), distribute (Dsj [z, x])]
-- Only distribute result if the result has changed compared to the original,
-- otherwise it will be an inifite loop
distribute (Dsj fs)
    | mfs /= fs = distribute (Dsj (mfs))
    | otherwise = Dsj (mfs)
    where mfs = map distribute fs
distribute (Cnj fs) = Cnj (map distribute fs)
distribute x = x

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
    -- Exercises from the workshop to test with
    print (cnf (head (parse "---1")))
    print (cnf (head (parse "-+(1 2)")))
    print (cnf (head (parse "-*(-1 -2)")))
    print (cnf (head (parse "((1 ==> 2) <=> (-2 ==> -1))")))

    -- -- Some other examples
    print (cnf (head (parse "+(1 *(1 2))")))
    print (cnf (head (parse "(1 ==> -+(2 -3))")))
    print (cnf (head (parse "+(-(1 ==> 2) (3 ==> 1))")))
    print (cnf (head (parse "+(*(1 2) 3)")))
    print (cnf (head (parse "(1<=>-3)")))
    print (cnf (head (parse "+(-3 (2<=>1))")))

