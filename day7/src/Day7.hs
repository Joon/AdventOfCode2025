module Day7
    ( indexLineChars
    , isCollision
    , splitCollisions
    , beam
    , stripChar
    ) where

import Data.List (sort, nub, (\\))
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

indexLineChars :: (Int, String) -> [(Int, Int, Char)]
-- Zip a repeating line index list, an infinite index list and all chars in the line
indexLineChars (lineIndex, lineVal) = zip3 [0..] (repeat lineIndex) lineVal

isCollision :: ([(Int, Int)], [(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
-- Take a list of beams and splitters. Find the locations where the beams
-- strike a splitter. For each beam, find the first splitter it hits (same X,
-- lowest Y that's greater than beam Y). Exclude collisions already found.
isCollision (beams, splitters, existingCollisions) = newCollisions
        where
                -- For each beam, find the first splitter it would hit
                findFirstCollision (beam_x, beam_y) =
                        case sort $ filter (\(sx, sy) -> sx == beam_x && sy > beam_y) splitters of
                                [] -> Nothing
                                (first:_) -> Just first
                -- Get first collision for each beam, remove Nothings
                allFirstCollisions = mapMaybe findFirstCollision beams
                -- Remove duplicates (multiple beams might hit same splitter)
                uniqueCollisions = nub allFirstCollisions
                -- Exclude any existing collisions
                newCollisions = uniqueCollisions \\ existingCollisions

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
