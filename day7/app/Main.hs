module Main where

import Day7 (indexLineChars, isCollision, splitCollisions, beam, stripChar)

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

