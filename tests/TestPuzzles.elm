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


testPuzzle maybePuzzle =
    case maybePuzzle of
        Nothing ->
            Expect.equal 2 1

        Just puzzle ->
            let
                ( p2d, actions ) =
                    Solve.getSolution puzzle.puzzle
            in
            PossibleList2d.toSudokuGrid p2d |> Expect.equal puzzle.solution


examples : Test
examples =
    describe "Examples"
        [ test "simple" <|
            \_ ->
                testPuzzle (Puzzles.puzzleFromName "2573")
        , test "easy" <|
            \_ ->
                testPuzzle (Puzzles.puzzleFromName "4291")
        , test "medium" <|
            \_ ->
                testPuzzle (Puzzles.puzzleFromName "8290")
        ]
