module Main where
import Data.List (sort, groupBy, (\\))
import Data.Function (on)
import Data.Either (rights)

indexLineChars :: (Int, String) -> [(Int, Int, Char)]
-- Zip a repeating line index list, an infinite index list and all chars in the line
indexLineChars (lineIndex, lineVal) = zip3 (repeat lineIndex) [0..] lineVal

isCollision :: ([(Int, Int)], [(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
-- Take a list of beams and splitters. Find the locations where the beams
-- strike a splitter. rules to strike a splitter: Splitter Y has to be larger 
-- than beam origin Y, Splitter X must be equal to beam X, the collision must
-- not yet be in the list. If there are duplicates, the lowest Y value is the
-- correct one
isCollision (beams, splitters, existingCollisions) = nubbed
        where
                -- First we find all possible collisions from the list
                plainCollisions = filter (\(splitter_x, splitter_y) -> 
                        any (\(beam_x, beam_y) -> beam_x == splitter_x && 
                        splitter_y > beam_y) beams) splitters
                -- Subtract any existing collisions from the list
                cleanedCollisions = sortedCollisions \\ existingCollisions
                -- Then we sort, ensuring that the Xes are together
                sortedCollisions = sort plainCollisions                                                        
                -- Now group by the xes, creating a list of lists of 
                -- tuples that share the same x value, sorted by y value. 
                -- Mapping head on to each of those lists returns the first 
                -- entry, i.e. the lowest Y value for the X value
                nubbed = map head . groupBy ((==) `on` fst) $ sortedCollisions

splitCollisions :: ([(Int, Int)], Int) -> [(Int, Int)]
splitCollisions (splitters, maxX) = lefts ++ rights
        where 
                lefts = [(x - 1, y) | (x, y) <- splitters, x >= 0]
                rights = [(x + 1, y) | (x, y) <- splitters, x <= maxX]

beam :: ([(Int, Int)], [(Int, Int)], [(Int, Int)], Int) -> ([(Int, Int)], [(Int, Int)])
beam (beams, splitters, collisions, maxX) 
        -- When we no longer see any new collisions, return all collusions and beams
        | null endNewCollisions = (allCollisions, allBeams)
        -- When there are collisions left, recurse to recalculate any new ones
        | otherwise             = beam (allBeams, splitters, allCollisions, maxX)
     where
        -- Find all collisions of a beam with a splitter
        newCollisions = isCollision (beams, collisions, splitters)
        -- Now add those to the existing collusion list
        allCollisions = newCollisions ++ collisions
        -- At each collision site, there are now two split beams
        newBeams = splitCollisions(collisions, maxX)
        -- Track the new beams
        allBeams = beams ++ newBeams
        -- Finally we see if, given the new beams and collisions, there are 
        -- any additional collisions
        endNewCollisions = isCollision (newBeams, splitters, allCollisions)

stripChar :: (Int, Int, Char) -> (Int, Int)
stripChar (first, second, third) = (first, second)

main :: IO ()
main = do
        contents <- readFile "./inputs/day7_test.txt"
        let zippedLines = zip [0..] (lines contents)
        let convertedLines = concatMap indexLineChars zippedLines
        let splitters = filter (\(_,_,c) -> c == '^') convertedLines
        let origin = head (filter (\(_,_,c) -> c == 'S') convertedLines)
        let origins = [stripChar origin]
        let splitterList = map stripChar splitters
        let maxX = maximum [x | (x, _, _) <- convertedLines]
        let (beams, collisions) = beam (origins, splitterList, [], maxX)
        print (length collisions)

