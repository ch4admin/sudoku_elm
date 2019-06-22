module TestPuzzles exposing (examples)

import Array
import Debug
import Expect exposing (Expectation)
import Grid exposing (get, getRow)
import PossibleList2d
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
                            ( p2d, actions ) =
                                Solve.getSolution puzzle.puzzle
                        in
                        PossibleList2d.toSudokuGrid p2d |> Expect.equal puzzle.solution

        --        , test "easy" <|
        --            \_ ->
        --                let
        --                    maybePuzzle =
        --                        Puzzles.puzzleFromName "4291"
        --                in
        --                case maybePuzzle of
        --                    Nothing ->
        --                        Expect.equal 2 1
        --
        --                    Just puzzle ->
        --                        let
        --                            ( possibleGrid, actions ) =
        --                                Solve.solution puzzle.puzzle
        --                        in
        --                        Expect.equal puzzle.solution (SudokuGrid.fromPossibleGrid possibleGrid)
        ]
