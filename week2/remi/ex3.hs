-- Started at 16:00, finished at 16:35

-- Descending strength list: [ex3a, ex3c, even, ex3b]
-- 'ex3c' and 'even' are equivalent, so we could also say:
-- [ex3a, even, ex3c, ex3b]

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Functions to check property strength from slides
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q
                    qp = stronger xs q p
                in
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"

-- Properties
ex3a, ex3b, ex3c :: Int -> Bool

ex3a = (\ x -> even x && x > 3)

ex3b = (\ x -> even x || x > 3)

ex3c = (\ x -> (even x && x > 3) || even x)

domain :: [Int]
domain = [(-10)..10]

main = do
    print "--ex3a vs even, ex3b, ex3c--"
    print (compar domain ex3a even)
    print (compar domain ex3a ex3b)
    print (compar domain ex3a ex3c)
    print "--ex3b vs even, ex3a, ex3c--"
    print (compar domain ex3b even)
    print (compar domain ex3b ex3a)
    print (compar domain ex3b ex3c)
    print "--ex3c vs even, ex3a, ex3b--"
    print (compar domain ex3c even)
    print (compar domain ex3c ex3a)
    print (compar domain ex3c ex3b)
    print "--even vs ex3a, ex3b, ex3c--"
    print (compar domain even ex3a)
    print (compar domain even ex3b)
    print (compar domain even ex3c)