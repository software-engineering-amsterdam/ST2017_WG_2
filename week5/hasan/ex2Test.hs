module Ex2Test where

import Ex1
import Ex2
import Lecture5
import Test.QuickCheck
import Data.Time.Clock
import Data.Time.Clock.POSIX

consistentHelper :: [Lecture5.Node] -> Bool
consistentHelper [] = True
consistentHelper ((s, xs):aa) = Lecture5.consistent s && consistentHelper aa

runEx2Test = do
    let sudokus = [Lecture5.example1, Lecture5.example2, Lecture5.example3, Lecture5.example4, Lecture5.example5]
    
    putStrLn "Ex1:"
    startTime1 <- getPOSIXTime
    let allConsistentEx1 = all (\sud -> consistentHelper (Lecture5.solveNs (Lecture5.initNode sud))) sudokus
    putStr "\tAll consistent:\n\t"
    print allConsistentEx1
    endTime1 <- getPOSIXTime
    putStr "\tTime taken:\n\t"
    print (endTime1 - startTime1)
    
    putStrLn "Ex2:"
    startTime2 <- getPOSIXTime
    let allConsistentEx2 = all (\sud -> consistentHelper (Lecture5.solveNs (Lecture5.initNode sud))) sudokus
    putStr "\tAll consistent:\n\t"
    print allConsistentEx2
    endTime2 <- getPOSIXTime
    putStr "\tTime taken:\n\t"
    print (endTime2 - startTime2)
    --let x = Lecture5.solveNs (Lecture5.initNode example7)
    --(sequence . fmap Lecture5.showNode) x
    --let y = Ex2.solveNs (Ex2.initNode example7)
    --(sequence . fmap Ex2.showNode) y
    --print (valid y)
    
    