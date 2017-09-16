-- Alphabet string from https://en.wikipedia.org/wiki/ROT13
-- Simple specification:
-- for every character in string x: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz 
-- it should map the position in x to y: NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm
-- so A becomes N, B becomes O...
-- every character not in x should be ignored, so special characters remain untouched


module Lab2Ex6 where

import Data.List
import Data.Char

keyString = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
valueString = "NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm"

rot13 :: [Char] -> String
rot13 "" = ""
rot13 n =   case elemIndex (head n) keyString of 
            Just keyPosInt -> [valueString !! keyPosInt] ++ (rot13 (tail n)) 
            Nothing -> [head n] ++ (rot13 (tail n))

main = do
    print "Encrypt 'Software Testing'"
    print (rot13 "Software Testing")
    print "Decrypt (Encrypt 'Software Testing')"
    print (rot13 (rot13 "Software Testing"))
    print "Encrypt 'Software Testing' == Decrypt (Encrypt 'Software Testing')"
    print ("Software Testing" == rot13 (rot13 "Software Testing"))