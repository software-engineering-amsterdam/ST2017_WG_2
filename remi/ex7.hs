-- Started at 10:25, finished at 12:15

module Lab1 where
import Data.List
import Test.QuickCheck

-- Taking the 10 modulus of n gives you its last digit, which we add to a list. Repeating this with n divided by 10
-- (as it's a integer division, this will 'drop' the last digit) until we get to zero, results in a conversion
-- of the integer to a list of it's digits.
-- Inspired by https://stackoverflow.com/a/3963286, which is in my opinion a very elegant way of achieving this.
-- I also looked at solutions that convert the integer to a string, but I like the fact that this solution
-- just uses integers, which is also more resource efficient.
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

-- Again, I decided not to use a solution that first turns the list into a string,
-- but decided to use mathematics instead. Inspired by https://stackoverflow.com/a/1918515,
-- when folding every partial result is multiplied by 10, and then the next number is added to it.
fromDigits :: [Integer] -> Integer
fromDigits x = foldl ((+) . (*10)) 0 x

maxNine :: Integer -> Integer
maxNine n = if n > 9 then (n - 9) else n

-- Double every second digit in the list.
-- First, I solved this using zipWith (*) n [1,2,1,2,1,2,1,2,1,2,1].
-- However, using a list like that does not look very elegant and also
-- causes the function only to work correctly with lists of odd length up to 11.
-- On this Stackoverflow answer https://stackoverflow.com/a/17383354, I found
-- using the cycle function would be a nice way to make the function work
-- for every list length.
doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond n = reverse (map maxNine (zipWith (*) (reverse n) (cycle [1,2])))

luhn :: Integer -> Bool
luhn n = sum (doubleEverySecond (toDigits n)) `mod` 10 == 0

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = length digits == 15 && validIin && luhn n
    where digits = toDigits n
          iin = fromDigits (take 2 digits)
          validIin = iin == 34 || iin == 37

isMaster :: Integer -> Bool
isMaster n = length digits == 16 && validIin && luhn n
    where digits = toDigits n
          iin = fromDigits (take 4 digits)
          iin2 = fromDigits (take 2 digits)
          validIin = (iin >= 2221 && iin <= 2720) || (iin2 >= 51 && iin2 <= 55)

isVisa :: Integer -> Bool
isVisa n = validLength && head digits == 4 && luhn n
    where digits = toDigits n
          cardLength = length digits
          validLength = cardLength == 13 || cardLength == 16 || cardLength == 19