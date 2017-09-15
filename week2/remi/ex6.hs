-- Rot13 took about 30 minutes, not tested yet

-- Specification:
-- Uppercase characters are changed into uppercase characters 13 places ahead
-- Lowercase characters are changed into lowercase characters 13 places ahead
-- Other characters are not changed

-- As we shift 13 characters, applying this twice should result into the original string.
-- Therefore, we can test the following property:
-- rot13 (rot13 x) = x

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
    | x `elem` ['A'..'Z'] = chr ((ord x - 65 + 13) `mod` 26 + 65)
    | x `elem` ['a'..'z'] = chr ((ord x - 97 + 13) `mod` 26 + 97)
    | otherwise = x

rot13 :: String -> String
rot13 x = map move13 x

-- Properties
propOwnInverse :: [Char] -> Bool
propOwnInverse x = x == rot13 (rot13 x)

main = do
    quickCheck propOwnInverse
