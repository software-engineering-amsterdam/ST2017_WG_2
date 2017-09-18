-- About 30min

module Lab3Ex2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3Ex1

testTuple :: (String, Form) -> Bool
testTuple (f1, f2) = (equiv f2 f3) where f3 = head (parse f1)

form_prop = p
form_neg = Neg p
form_cnj = Cnj [p, q]
form_dsj = Dsj [p, q]
form_impl = Impl (Neg (Neg p)) p
form_equiv = Equiv (Neg (Neg p)) p
form_ = Equiv (Impl p q) (Impl (Neg q) (Neg p))

tuples =    [
                ("1", form_prop),
                ("-1", form_neg),
                ("*(1 2)", form_cnj),
                ("+(1 2)", form_dsj),
                ("(--1 ==> 1)", form_impl),
                ("(--1 <=> 1)", form_equiv),
                ("((1==>2)<=>(-2==>-1))", form_)
            ]

mainLab3Ex2 = do
    all (\x -> testTuple x) tuples