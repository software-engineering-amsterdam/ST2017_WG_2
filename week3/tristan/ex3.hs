-- This took ~4 hours

module Ex3 (cnf) where

import Debug.Trace
import Control.Monad
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Ex1

cnf :: Form -> Form
cnf forms
    | tautology forms     = Dsj [Prop propName, Neg (Prop propName)]
    | contradiction forms = Cnj [Prop propName, Neg (Prop propName)]
                            where propName = head (propNames forms)
cnf (Prop prop)           = Prop prop
cnf (Neg (Prop prop))     = Neg (Prop prop)
cnf (Neg (Neg form))      = cnf form
cnf (Neg (Cnj forms))     = Dsj (map (cnf . Neg) forms) 
cnf (Neg (Dsj forms))     = Cnj (map (cnf . Neg) forms)
cnf (Neg form)            = Neg (cnf form)
cnf (Cnj [Prop p])        = Prop p
cnf (Dsj [Prop p])        = Prop p
cnf (Cnj forms)           = Cnj (map cnf (nub forms))
cnf (Dsj forms)           = dsjTransform (Dsj (map cnf (nub forms)))
cnf (Impl left right)     = cnf (Dsj [Neg (cnf left), cnf right])
cnf (Equiv left right)    = cnf (Dsj [Cnj [left', right'], Cnj [Neg left', Neg right']])
                            where left'  = cnf left
                                  right' = cnf right

-- Distributes ORs inwards over ANDs
dsjTransform :: Form -> Form
dsjTransform (Dsj forms) = case conjunction of
    Just (Cnj conjunction) -> do
        let other = forms \\ [Cnj conjunction]
        Cnj (map (\cnjElem -> Dsj (cnjElem:other)) conjunction)
    Nothing -> Dsj forms
    where conjunction = find (isCnj) forms
dsjTransform x = x

isCnj :: Form -> Bool
isCnj (Cnj _) = True
isCnj _ = False

main = do
    let form = head (parse "(1 ==> -+(2 -3))")
    print (cnf form)