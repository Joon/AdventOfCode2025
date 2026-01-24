module Main where

import Test.Hspec
import Data.List (sort)
import Day7 (indexLineChars, isCollision, splitCollisions, beam, stripChar)

main :: IO ()
main = hspec $ do
    describe "indexLineChars" $ do
        it "indexes an empty string" $ do
            indexLineChars (0, "") `shouldBe` []

        it "indexes a single character" $ do
            indexLineChars (0, "a") `shouldBe` [(0, 0, 'a')]

        it "indexes multiple characters on line 0" $ do
            indexLineChars (0, "abc") `shouldBe` [(0, 0, 'a'), (1, 0, 'b'), (2, 0, 'c')]

        it "indexes characters on a different line" $ do
            indexLineChars (5, "xy") `shouldBe` [(0, 5, 'x'), (1, 5, 'y')]

        it "handles special characters" $ do
            indexLineChars (2, "^S.") `shouldBe` [(0, 2, '^'), (1, 2, 'S'), (2, 2, '.')]

    describe "stripChar" $ do
        it "strips character from tuple" $ do
            stripChar (1, 2, 'a') `shouldBe` (1, 2)

        it "works with different values" $ do
            stripChar (10, 20, '^') `shouldBe` (10, 20)

        it "works with zero values" $ do
            stripChar (0, 0, 'S') `shouldBe` (0, 0)

    describe "splitCollisions" $ do
        it "returns empty for empty splitters" $ do
            splitCollisions ([], 10) `shouldBe` []

        it "creates left and right beams from single splitter" $ do
            sort (splitCollisions ([(5, 3)], 10)) `shouldBe` sort [(4, 3), (6, 3)]

        it "creates beams from multiple splitters" $ do
            let result = splitCollisions ([(5, 3), (7, 4)], 10)
            sort result `shouldBe` sort [(4, 3), (6, 3), (6, 4), (8, 4)]

        it "filters left beams at x=0 boundary" $ do
            let result = splitCollisions ([(0, 5)], 10)
            -- x=0 should still produce left beam at x=-1 since x >= 0 is true
            sort result `shouldBe` sort [(1, 5)]

        it "filters right beams at maxX boundary" $ do
            let result = splitCollisions ([(10, 5)], 10)
            -- x=10 should still produce right beam at x=11 since x <= 10 is true
            sort result `shouldBe` sort [(9, 5)]

        it "handles splitter at x=1" $ do
            let result = splitCollisions ([(1, 2)], 10)
            sort result `shouldBe` sort [(0, 2), (2, 2)]

    describe "isCollision" $ do
        it "returns empty when no splitters" $ do
            isCollision ([(0, 0)], [], []) `shouldBe` []

        it "returns empty when no beams" $ do
            isCollision ([], [(5, 5)], []) `shouldBe` []

        it "finds collision when beam x matches splitter x and splitter y > beam y" $ do
            let beams = [(3, 0)]
            let splitters = [(3, 5)]
            isCollision (beams, splitters, []) `shouldBe` [(3, 5)]

        it "no collision when splitter y <= beam y" $ do
            let beams = [(3, 5)]
            let splitters = [(3, 3)]
            isCollision (beams, splitters, []) `shouldBe` []

        it "no collision when x values differ" $ do
            let beams = [(3, 0)]
            let splitters = [(5, 5)]
            isCollision (beams, splitters, []) `shouldBe` []

        it "finds multiple collisions" $ do
            let beams = [(3, 0), (5, 0)]
            let splitters = [(3, 5), (5, 7)]
            let result = isCollision (beams, splitters, [])
            sort result `shouldBe` sort [(3, 5), (5, 7)]

        it "returns lowest y for same x when multiple splitters" $ do
            let beams = [(3, 0)]
            let splitters = [(3, 5), (3, 10), (3, 3)]
            let result = isCollision (beams, splitters, [])
            result `shouldBe` [(3, 3)]

        it "handles existing collisions parameter" $ do
            let beams = [(3, 0)]
            let splitters = [(3, 5)]
            let existing = [(3, 5)]
            -- Note: existing collisions is the third parameter
            isCollision (beams, splitters, existing) `shouldBe` []

        it "finds separate collisions for beams at same x but different y" $ do
            -- Beam at (5, 0) should hit (5, 2), beam at (5, 4) should hit (5, 6)
            let beams = [(5, 0), (5, 4)]
            let splitters = [(5, 2), (5, 6)]
            let result = isCollision (beams, splitters, [])
            sort result `shouldBe` sort [(5, 2), (5, 6)]

        it "beam below a splitter should not hit it" $ do
            -- Beam at (5, 5) should NOT hit splitter at (5, 3)
            let beams = [(5, 5)]
            let splitters = [(5, 3), (5, 8)]
            let result = isCollision (beams, splitters, [])
            result `shouldBe` [(5, 8)]

        it "each beam finds its own first collision" $ do
            -- Beam at (5, 0) should hit (5, 3), not (5, 7)
            -- Beam at (5, 5) should hit (5, 7)
            let beams = [(5, 0), (5, 5)]
            let splitters = [(5, 3), (5, 7)]
            let result = isCollision (beams, splitters, [])
            sort result `shouldBe` sort [(5, 3), (5, 7)]

    describe "beam" $ do
        it "returns empty collisions when no splitters" $ do
            let (beams, collisions) = beam ([(0, 0)], [], [], 10)
            collisions `shouldBe` []

        it "returns original beam when no splitters" $ do
            let (beams, collisions) = beam ([(5, 0)], [], [], 10)
            beams `shouldBe` [(5, 0)]

        it "handles single beam single splitter" $ do
            let origins = [(5, 0)]
            let splitters = [(5, 3)]
            let (beams, collisions) = beam (origins, splitters, [], 10)
            -- Should find collision at (5, 3)
            (5, 3) `elem` collisions `shouldBe` True

        it "handles empty beams" $ do
            let (beams, collisions) = beam ([], [(5, 5)], [], 10)
            collisions `shouldBe` []
            beams `shouldBe` []

        it "handles multiple origins" $ do
            let origins = [(3, 0), (7, 0)]
            let splitters = [(3, 5), (7, 5)]
            let (beams, collisions) = beam (origins, splitters, [], 10)
            length collisions `shouldBe` 2

        it "traverses the input grid for multiple collisions" $ do
            let (beams, collisions) = beam ([(0, 0)], [(0, 5), (1, 9)], [], 10)
            sort collisions `shouldBe` sort [(1, 9), (0, 5)]

        it "split beams find collisions below their split point" $ do
            -- Beam starts at (5, 0), hits splitter at (5, 2), splits to (4, 2) and (6, 2)
            -- Those split beams should hit splitters at (4, 5) and (6, 5)
            let origins = [(5, 0)]
            let splitters = [(5, 2), (4, 5), (6, 5)]
            let (_, collisions) = beam (origins, splitters, [], 10)
            sort collisions `shouldBe` sort [(5, 2), (4, 5), (6, 5)]

        it "handles multiple splitters in same column at different depths" $ do
            -- Beam at (5, 0) hits (5, 3), splits. Later a split beam returns to x=5
            -- and should hit (5, 7) if it's below the beam's y position
            let origins = [(5, 0)]
            let splitters = [(5, 3), (4, 5), (6, 5), (5, 7)]
            let (_, collisions) = beam (origins, splitters, [], 10)
            -- (5,3) is hit first, splits to (4,3) and (6,3)
            -- (4,3) hits (4,5), splits to (3,5) and (5,5)
            -- (6,3) hits (6,5), splits to (5,5) and (7,5)
            -- (5,5) beams should hit (5,7)
            (5, 7) `elem` collisions `shouldBe` True

        it "beam at edge only splits one direction" $ do
            let origins = [(0, 0)]
            let splitters = [(0, 3), (1, 5)]
            let (_, collisions) = beam (origins, splitters, [], 10)
            -- Beam at x=0 hits (0, 3), can only split right to (1, 3)
            -- (1, 3) should hit (1, 5)
            sort collisions `shouldBe` sort [(0, 3), (1, 5)]

        it "handles duplicate beams arriving at same position" $ do
            -- Two paths converge at the same point, should still find collision
            let origins = [(4, 0), (6, 0)]
            let splitters = [(4, 2), (6, 2), (5, 5)]
            let (_, collisions) = beam (origins, splitters, [], 10)
            -- Both (4,0) and (6,0) hit their respective splitters
            -- Both create beams at (5, 2) which should hit (5, 5)
            -- But (5, 5) should only be counted once
            sort collisions `shouldBe` sort [(4, 2), (6, 2), (5, 5)]
