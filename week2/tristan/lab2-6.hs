-- This took ~20 minutes

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (c:s) = rot13char c : rot13 s

rot13char :: Char -> Char
rot13char c
    | elem c lowercase = rot13InList c lowercase
    | elem c uppercase = rot13InList c uppercase
    | otherwise = c
    where lowercase = ['a'..'z']
          uppercase = ['A'..'Z']

rot13InList :: Char -> [Char] -> Char
rot13InList c l
    | o > ord (last l) = chr (o - 26)
    | otherwise = chr o
    where o = (ord c) + 13

main = do
    print (rot13 "hallo wereld")
