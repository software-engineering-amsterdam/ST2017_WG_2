-- TEST REPORT
-- Shoutout to Vincent for discovering the command ":set +s"
--
-- *Ex2Test> :set +s
-- *Ex2Test> runWithEx1Test
-- Ex1:
--         All consistent:
--         True
-- (0.30 secs, 12,887,936 bytes)
-- *Ex2Test> runWithEx2Test
-- Ex2:
--         All consistent:
--         True
-- (0.03 secs, 12,868,528 bytes)
-- *Ex2Test>

-- Above shows the correctness of both solutions as well the effiency with memory and speed

module Ex2Test where

import Ex1
import Ex2
import Lecture5
import Test.QuickCheck

consistentHelper :: [Lecture5.Node] -> Bool
consistentHelper [] = True
consistentHelper ((s, xs):aa) = Lecture5.consistent s && consistentHelper aa

sudokus = [Lecture5.example1, Lecture5.example2, Lecture5.example3, Lecture5.example4]

runWithEx2Test = do
    putStrLn "Ex2:"
    let allConsistent = all (\sud -> consistentHelper (Lecture5.solveNs (Lecture5.initNode sud))) sudokus
    putStr "\tAll consistent:\n\t"
    print allConsistent
    
runWithEx1Test = do
    putStrLn "Ex1:"
    let allConsistent = all (\sud -> consistentHelper (Lecture5.solveNs (Lecture5.initNode sud))) sudokus
    putStr "\tAll consistent:\n\t"
    print allConsistent