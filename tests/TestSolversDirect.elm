module TestSolversDirect exposing (testSolversDirect)

import Array
import Expect exposing (Expectation)
import Grid exposing (get)
import PossibleList2d exposing (PossibleCell(..), Rationale(..), Removal)
import Set
import SolversDirectLogic
import SudokuGrid
    exposing
        ( SudokuGrid
        )
import Test exposing (..)


testSolversDirect : Test
testSolversDirect =
    describe "Test Solvers Direct"
        [ test "removeFilledInSameRow" <|
            \_ ->
                let
                    p2d =
                        PossibleList2d.fromSudokuGrid (SudokuGrid.fromListOfString [ "1." ])

                    expected =
                        [ [ Filled 1, Possibles { remaining = Set.fromList [ 2, 3, 4, 5, 6, 7, 8, 9 ], removed = [ Removal [ 1 ] SameRow ] } ]
                        ]
                in
                SolversDirectLogic.removeFilledInSameRow p2d |> Expect.equal expected
        , test "removeFilledInSameCol" <|
            \_ ->
                let
                    p =
                        PossibleList2d.fromSudokuGrid (SudokuGrid.fromListOfString [ "1", "." ])

                    expected =
                        [ [ Filled 1 ]
                        , [ Possibles { remaining = Set.fromList [ 2, 3, 4, 5, 6, 7, 8, 9 ], removed = [ Removal [ 1 ] SameColumn ] } ]
                        ]
                in
                SolversDirectLogic.removeFilledInSameColumn p |> Expect.equal expected
        , test "removeFilledInSameBox" <|
            \_ ->
                let
                    p =
                        PossibleList2d.fromSudokuGrid (SudokuGrid.fromListOfString [ "123", "4.6", "789" ])

                    expected =
                        [ [ Filled 1, Filled 2, Filled 3 ]
                        , [ Filled 4, Possibles { remaining = Set.fromList [ 5 ], removed = [ Removal [ 1, 2, 3, 4, 6, 7, 8, 9 ] SameBox ] }, Filled 6 ]
                        , [ Filled 7, Filled 8, Filled 9 ]
                        ]
                in
                SolversDirectLogic.removeFilledInSameBox p |> Expect.equal expected
        ]
