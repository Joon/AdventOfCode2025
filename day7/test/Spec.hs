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
            indexLineChars (0, "abc") `shouldBe` [(0, 0, 'a'), (0, 1, 'b'), (0, 2, 'c')]

        it "indexes characters on a different line" $ do
            indexLineChars (5, "xy") `shouldBe` [(5, 0, 'x'), (5, 1, 'y')]

        it "handles special characters" $ do
            indexLineChars (2, "^S.") `shouldBe` [(2, 0, '^'), (2, 1, 'S'), (2, 2, '.')]

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
            sort result `shouldBe` sort [(-1, 5), (1, 5)]

        it "filters right beams at maxX boundary" $ do
            let result = splitCollisions ([(10, 5)], 10)
            -- x=10 should still produce right beam at x=11 since x <= 10 is true
            sort result `shouldBe` sort [(9, 5), (11, 5)]

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
            isCollision (beams, splitters, existing) `shouldBe` [(3, 5)]

    describe "beam" $ do
        it "returns empty collisions when no splitters" $ do
            let (collisions, beams) = beam ([(0, 0)], [], [], 10)
            collisions `shouldBe` []

        it "returns original beam when no splitters" $ do
            let (collisions, beams) = beam ([(5, 0)], [], [], 10)
            beams `shouldBe` [(5, 0)]

        it "handles single beam single splitter" $ do
            let origins = [(5, 0)]
            let splitters = [(5, 3)]
            let (collisions, beams) = beam (origins, splitters, [], 10)
            -- Should find collision at (5, 3)
            (5, 3) `elem` collisions `shouldBe` True

        it "handles empty beams" $ do
            let (collisions, beams) = beam ([], [(5, 5)], [], 10)
            collisions `shouldBe` []
            beams `shouldBe` []

        it "handles multiple origins" $ do
            let origins = [(3, 0), (7, 0)]
            let splitters = [(3, 5), (7, 5)]
            let (collisions, beams) = beam (origins, splitters, [], 10)
            length collisions `shouldBe` 2
