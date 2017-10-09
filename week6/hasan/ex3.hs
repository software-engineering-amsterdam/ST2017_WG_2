module Ex3 where

import Lecture6

compositesCustom :: [Integer]
compositesCustom = [x | x <- [1..], not (prime x)]