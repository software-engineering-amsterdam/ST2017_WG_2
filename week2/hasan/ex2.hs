-- 1.5 hour

module Lab2 where
import Data.List

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
                    | a <= 0 || b <= 0 || c <= 0 = NoTriangle
                    | a == b && b == c = Equilateral
                    | aa*aa + bb*bb == cc*cc = Rectangular
                    | aa == bb || bb == cc = Isosceles
                    | otherwise = Other
                    where [aa,bb,cc] = (sort [a, b, c])
                    

main = do
    print "NoTriangle:"
    print (triangle 0 1 2)
    print "Equilateral:"
    print (triangle 1 1 1)
    print "Rectangular:"
    print (triangle 3 4 5)
    print "Isosceles:"
    print (triangle 1 2 2)
    print "Other:"
    print (triangle 1 2 3)