-- About 2 hours, old implementation

-- Import libraries
import Data.List

-- Define boys
data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- If boy a accuses boy b, then a must be in the accusers list of b
accuses :: Boy -> Boy -> Bool
accuses a b = elem a (accusers b)

-- Accusers lists of all boys
accusers :: Boy -> [Boy]
accusers Matthew = [Peter, Jack, Arnold]
accusers Peter = [Matthew, Jack]
accusers Jack = [Carl]
accusers Arnold = [Peter, Arnold, Matthew]
accusers Carl = [Jack, Carl]

-- Assume the given set of boys is guilty, return how many boys are saying the truth
getHonest :: [Boy] -> [Boy]
getHonest [] = boys
getHonest n = filter (\x -> length(n) == length(intersect (accusers x) n)) boys

guilty, honest :: [Boy]
-- For every possible combination of guilty boys, we return the first combination of guilty boys that would have 3 honest people
guilty = head(filter (\possibleGuiltyPeople -> length(getHonest possibleGuiltyPeople) == 3)  (subsequences boys))
-- Filter boys that include the guilty people in their list of accusers
honest = filter (\x -> length(intersect guilty (accusers x)) == length(guilty)) boys

main = do
    print "Guilty boys:"
    print guilty
    print "Honest boys:"
    print honest