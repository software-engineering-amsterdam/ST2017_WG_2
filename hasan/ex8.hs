-- About 2 hours to write my old solution which had written out lists of accused boys
-- About 30min to rewrite it to this solution

-- Import libraries
import Data.List

-- Define boys
data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Boolean formulas
accuses :: Boy -> Boy -> Bool
accuses Matthew x = not (elem x [Carl, Matthew])
accuses Peter x = elem x [Matthew, Jack]
accuses Jack x = not(accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = (accuses Matthew x) /= (accuses Peter x)
accuses Carl x = not(accuses Arnold x)

-- Accusers lists of all boys
accusers :: Boy -> [Boy]
accusers n = filter (\x -> accuses x n) boys

-- Get all posssible combinations of answers: [[], [Matthew], [Peter], ..., [Matthew, Peter, Jack, Arnold, Carl]]
answers :: [[Boy]]
answers = subsequences boys

-- Given a list of boys, get the boys that accuses all those in the list
getHonest :: [Boy] -> [Boy]
getHonest answer = filter (\x -> all (\y -> accuses x y) answer) boys

guilty, honest :: [Boy]
-- For every possible answer we are going to check which answer has 3 boys that accuse everyone in that answer
guilty = head (filter (\answer -> length(getHonest answer) == 3) answers)
-- Honest are those who accused everyone in the list of guilty
honest = getHonest guilty

main = do
    print "Guilty"
    print guilty
    print "Honest"
    print honest