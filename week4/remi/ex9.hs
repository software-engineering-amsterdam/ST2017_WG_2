-- Started 11:15

module Lab4 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture4

-- Print statements in a more readable way, using common imperative language syntax
instance Show Statement where
    show (Ass v e) = show v ++ " = " ++ show e ++ ";"
    show (Cond c s1 s2) = "if (" ++ show c ++ ") {\n" ++ show s1 ++ "} else {\n" ++ show s2 ++ "}"
    show (Seq s) = concat (map (\x -> show x ++ "\n") s)
    show (While c s) = "while (" ++ show c ++ ") {\n" ++ show s ++ "}"

test :: Statement
test = Seq [Ass "x" (I 0), Ass "y" (I 1), Ass "z" (I 2),
           Cond (Gt (V "n") (I 0))
             (Seq [Ass "z" (V "x"),
                   Ass "x" (V "y"),
                   Ass "y" (Add (V "z") (V "y")),
                   Ass "n" (Subtr (V "n") (I 1))])
             (Seq [Ass "z1" (V "x1"),
                   Ass "x1" (V "y1"),
                   Ass "y1" (Add (V "z1") (V "y1")),
                   Ass "n1" (Subtr (V "n1") (I 1))])]

main = print test