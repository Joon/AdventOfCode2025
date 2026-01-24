module Day7
    ( indexLineChars
    , isCollision
    , splitCollisions
    , beam
    , stripChar
    ) where

import Data.List (sort, groupBy, (\\))
import Data.Function (on)
import Debug.Trace (trace)

indexLineChars :: (Int, String) -> [(Int, Int, Char)]
-- Zip a repeating line index list, an infinite index list and all chars in the line
indexLineChars (lineIndex, lineVal) = zip3 [0..] (repeat lineIndex) lineVal

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
                -- Then we sort, ensuring that the Xes are together
                sortedCollisions = sort plainCollisions
                -- Subtract any existing collisions from the list
                cleanedCollisions = sortedCollisions \\ existingCollisions
                -- Now group by the xes, creating a list of lists of
                -- tuples that share the same x value, sorted by y value.
                -- Mapping head on to each of those lists returns the first
                -- entry, i.e. the lowest Y value for the X value
                nubbed = map head . groupBy ((==) `on` fst) $ cleanedCollisions

splitCollisions :: ([(Int, Int)], Int) -> [(Int, Int)]
splitCollisions (splitters, maxX) = lefts ++ rights
        where
                lefts = [(x - 1, y) | (x, y) <- splitters, x > 0]
                rights = [(x + 1, y) | (x, y) <- splitters, x < maxX]

beam :: ([(Int, Int)], [(Int, Int)], [(Int, Int)], Int) -> ([(Int, Int)], [(Int, Int)])
beam (beams, splitters, collisions, maxX)
        -- When we no longer see any new collisions, return all collusions and beams
        | null endNewCollisions = (allBeams, allCollisions)
        -- When there are collisions left, recurse to recalculate any new ones
        | otherwise             = beam (newBeams, splitters, allCollisions, maxX)
     where
        -- Find all collisions of a beam with a splitter
        newCollisions = isCollision (beams, splitters, collisions)
        -- Now add those to the existing collusion list
        allCollisions = newCollisions ++ collisions
        -- At each collision site, there are now two split beams
        newBeams = splitCollisions(newCollisions, maxX)
        -- Track the new beams
        allBeams = beams ++ newBeams
        -- Finally we see if, given the new beams and collisions, there are
        -- any additional collisions
        endNewCollisions = isCollision (newBeams, splitters, allCollisions)

stripChar :: (Int, Int, Char) -> (Int, Int)
stripChar (first, second, _) = (first, second)
