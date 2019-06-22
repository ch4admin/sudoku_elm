module TestAction exposing (testSolvers)

import Action exposing (Action)
import Array
import Expect exposing (Expectation)
import Grid exposing (get)
import PossibleList2d exposing (PossibleCell(..))
import Set
import SudokuGrid
    exposing
        ( SudokuGrid
        )
import Test exposing (..)


testSolvers : Test
testSolvers =
    describe "Test Solvers"
        [ test "getActionFromCell filled" <|
            \_ ->
                Action.getActionFromCell 1 1 (Filled 4) |> Expect.equal Nothing
        , test "getActionFromCell multiple" <|
            \_ ->
                Action.getActionFromCell 1 1 (Possibles { remaining = Set.fromList [ 1, 2 ], removed = [] }) |> Expect.equal Nothing
        , test "getActionFromCell one" <|
            \_ ->
                Action.getActionFromCell 1 1 (Possibles { remaining = Set.fromList [ 2 ], removed = [] }) |> Expect.equal (Just (Action 1 1 2 []))
        , test "getAction" <|
            \_ ->
                let
                    input =
                        [ [ Filled 7, Possibles { remaining = Set.fromList [ 2 ], removed = [] } ] ]
                in
                Action.getAction input |> Expect.equal (Just (Action 0 1 2 []))
        ]
