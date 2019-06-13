module TestExamples exposing (examples)

import Array
import Debug
import Expect exposing (Expectation)
import Grid exposing (Coordinate, get, getRow)
import Puzzles
import Solve
import SudokuGrid
import Test exposing (..)


examples : Test
examples =
    describe "Examples"
        [ test "simple" <|
            \_ ->
                let
                    maybePuzzle =
                        Puzzles.puzzleFromName "2573"
                in
                case maybePuzzle of
                    Nothing ->
                        Expect.equal 2 1

                    Just puzzle ->
                        let
                            ( possibleGrid, actions ) =
                                Solve.solution puzzle.puzzle
                        in
                        Expect.equal puzzle.solution (SudokuGrid.fromPossibleGrid possibleGrid)
        , test "easy" <|
            \_ ->
                let
                    maybePuzzle =
                        Puzzles.puzzleFromName "4291"
                in
                case maybePuzzle of
                    Nothing ->
                        Expect.equal 2 1

                    Just puzzle ->
                        let
                            ( possibleGrid, actions ) =
                                Solve.solution puzzle.puzzle
                        in
                        Expect.equal puzzle.solution (SudokuGrid.fromPossibleGrid possibleGrid)
        ]
