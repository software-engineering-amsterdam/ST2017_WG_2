-- problem 56

module Ex10a where 

problemNumbers :: [Integer]
problemNumbers = [a^b | a <- [1..100], b <- [1..100]]

digits :: Integer -> [Integer]
digits = map (read . return) . show

main = do
    let maxSum = maximum (map sum (map digits problemNumbers))
    print (maxSum)