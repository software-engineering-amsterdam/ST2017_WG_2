-- About 1 hour

-- Learn syntax of matching the first two elements of the list:
-- https://stackoverflow.com/questions/18774485/haskell-function-to-add-first-two-numbers-of-list

-- Learn cartessian product syntax:
-- https://stackoverflow.com/questions/4119730/cartesian-product-of-2-lists-in-haskell

pythagorean :: [Float] -> Float
pythagorean [] = 0
pythagorean x = sum (map (\y -> y*y) x)

-- Find all combination of pairs where numbers are between 1 and 1000 (inclusive)
numbers n = [[x,y] | x <- [1..n], y <- [1..n]]
-- Filter those where a is bigger than b
numbers2 n = filter (\x -> head x <= head (tail x)) (numbers n)

-- Finds a and b where a*a + b*b = n
findSolution :: Float -> [Float]
findSolution n = head (filter (\x -> (sum x) + sqrt (pythagorean x) == n) (numbers2 n))

-- Converts [a, b] to [a, b, c]
convertABToABC :: [Float] -> [Float]
convertABToABC (a:b:xs) = [a, b, sqrt (pythagorean [a, b])]
                        
main = do
    print (convertABToABC (findSolution 1000.0))