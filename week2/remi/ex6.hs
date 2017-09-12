-- Rot13 took about 30 minutes, not tested yet

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- ord turns the enum of a char into it's integer value, chr the other way around
-- transforming the character to zero based makes it possible to apply modulo
-- 65 = A, 97 = a
move13 :: Char -> Char
move13 x
    | isUpper x = chr ((ord x - 65 + 13) `mod` 26 + 65)
    | isLower x = chr ((ord x - 97 + 13) `mod` 26 + 97)
    | otherwise = x

rot13 :: String -> String
rot13 x = map move13 x

-- Properties
propOwnInverse :: String -> Bool
propOwnInverse x = x == rot13 (rot13 x)

main = do
    -- Need a way to only generate readable strings
    quickCheck propOwnInverse