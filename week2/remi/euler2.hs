module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

fibs :: Integer -> Integer -> [Integer]
fibs a b = [a] ++ (fibs b (a+b))

fibonacci :: [Integer]
fibonacci = fibs 1 2

euler2 :: Integer
euler2 = sum xs
    where f = filter even fibonacci
          xs = takeWhile (\x -> x <= 4000000) f
