-- ID: 11408227
-- Name: Vincent Jong
-- Time: 15:10 - 15:55 (Programming), 15:55 - 16:05, 10:40 -  (Testing), (Answering)

import Data.List
import Test.QuickCheck

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

isNoTriangle :: Integer -> Integer -> Integer -> Bool
isNoTriangle a b c = a <= 0 || b <= 0 || c <= 0

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c
    | a ^ 2 + b ^ 2 == c ^ 2 = True
    | a ^ 2 + c ^ 2 == b ^ 2 = True
    | b ^ 2 + c ^ 2 == a ^ 2 = True
    | otherwise = False

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = a == b && b == c

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c
    | a == b && not (a == c) && not (b == c) = True
    | a == c && not (a == b) && not (b == c) = True
    | b == c && not (a == b) && not (a == c) = True
    | otherwise = False

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c 
    | isNoTriangle a b c = NoTriangle
    | isRectangular a b c = Rectangular
    | isEquilateral a b c = Equilateral
    | isIsosceles a b c = Isosceles
    | otherwise = Other

testRel :: (a -> a -> Bool) -> (a -> a) -> [a] -> Bool
testRel spec f = all (\x -> spec x (f x))

testInvar :: Eq b => (a -> b) -> (a -> a) -> [a] -> Bool
testInvar specf = testRel (\ x y -> specf x == specf y)

main = do
    putStrLn "Testing no triangles\n"
    print $ triangle 1 0 1 == NoTriangle
    print $ triangle 0 0 1 == NoTriangle
    print $ triangle (-5) 1 4 == NoTriangle
    putStrLn "\nTesting rectangular triangles\n"
    print $ triangle 3 4 5 == Rectangular
    print $ triangle 6 8 10 == Rectangular
    putStrLn "\nTesting equilateral triangles\n"
    print $ triangle 25 25 25 == Equilateral
    print $ triangle 7 7 7 == Equilateral
    print $ triangle 42 42 42 == Equilateral
    putStrLn "\nTesting isosceles triangles\n"
    print $ triangle 8 8 5 == Isosceles
    print $ triangle 3 1 3 == Isosceles
    print $ triangle 66 42 42 == Isosceles
    putStrLn "\nTesting other triangles\n"
    print $ triangle 251 253 257 == Other
    print $ triangle 53 62 91 == Other
    print $ triangle 4 7 1 == Other