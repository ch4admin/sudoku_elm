module TestSolvers exposing (testSolvers)

import Array
import Expect exposing (Expectation)
import Grid exposing (get)
import Set
import Solvers
import SudokuGrid
    exposing
        ( Action
        , PossibleCell(..)
        , PossibleGrid
        , Rationale(..)
        , Removal
        , SudokuGrid
        )
import Test exposing (..)


testSolvers : Test
testSolvers =
    describe "Test Solvers"
        [ test "getActionFromCell filled" <|
            \_ ->
                Expect.equal (Solvers.actionFromCellWithOnePossible 1 1 (Filled 4)) Nothing
        , test "getActionFromCell multiple" <|
            \_ ->
                Expect.equal (Solvers.actionFromCellWithOnePossible 1 1 (Possibles { remaining = Set.fromList [ 1, 2 ], removed = [] })) Nothing
        , test "getActionFromCell one" <|
            \_ ->
                Expect.equal (Solvers.actionFromCellWithOnePossible 1 1 (Possibles { remaining = Set.fromList [ 2 ], removed = [] })) (Just (Action 1 1 2 []))
        , test "getActionFromIndexedCells" <|
            \_ ->
                let
                    input =
                        [ ( 1, 1, Filled 7 ), ( 1, 2, Possibles { remaining = Set.fromList [ 2 ], removed = [] } ) ]
                in
                Expect.equal (Just (Action 1 2 2 [])) (Solvers.getActionFromIndexedCells input)
        , test "removeSameRow" <|
            \_ ->
                let
                    p =
                        SudokuGrid.initPossibleGrid (SudokuGrid.fromListOfString [ "1." ])

                    expected =
                        Grid.fromList
                            [ [ Filled 1, Possibles { remaining = Set.fromList [ 2, 3, 4, 5, 6, 7, 8, 9 ], removed = [ Removal [ 1 ] SameRow ] } ]
                            ]
                in
                Expect.equal expected (Solvers.removeSameRow p)
        , test "removeSameCol" <|
            \_ ->
                let
                    p =
                        SudokuGrid.initPossibleGrid (SudokuGrid.fromListOfString [ "1", "." ])

                    expected =
                        Grid.fromList
                            [ [ Filled 1 ]
                            , [ Possibles { remaining = Set.fromList [ 2, 3, 4, 5, 6, 7, 8, 9 ], removed = [ Removal [ 1 ] SameColumn ] } ]
                            ]
                in
                Expect.equal expected (Solvers.removeSameCol p)
        , test "removeSameBox" <|
            \_ ->
                let
                    p =
                        SudokuGrid.initPossibleGrid (SudokuGrid.fromListOfString [ "123", "4.6", "789" ])

                    expected =
                        Grid.fromList
                            [ [ Filled 1, Filled 2, Filled 3 ]
                            , [ Filled 4, Possibles { remaining = Set.fromList [ 5 ], removed = [ Removal [ 1, 2, 3, 4, 6, 7, 8, 9 ] SameBox ] }, Filled 6 ]
                            , [ Filled 7, Filled 8, Filled 9 ]
                            ]
                in
                Expect.equal expected (Solvers.removeSameBox p)
        , test "ValueOnlyPossibleInOneCellInRow" <|
            \_ ->
                let
                    init =
                        Grid.fromList
                            [ [ Filled 1
                              , Possibles { remaining = Set.fromList [ 2, 3, 4, 5, 6, 7, 8, 9 ], removed = [ Removal [ 1 ] SameRow ] }
                              , Possibles { remaining = Set.fromList [ 4, 5, 6, 7, 8, 9 ], removed = [ Removal [ 1 ] SameRow ] }
                              , Possibles { remaining = Set.fromList [ 3, 4 ], removed = [ Removal [ 1 ] SameRow ] }
                              ]
                            ]

                    expected =
                        Grid.fromList
                            [ [ Filled 1
                              , Possibles { remaining = Set.fromList [ 2 ], removed = [ Removal [ 1 ] SameRow, Removal [ 3, 4, 5, 6, 7, 8, 9 ] ValueOnlyPossibleInOneCellInRow ] }
                              , Possibles { remaining = Set.fromList [ 4, 5, 6, 7, 8, 9 ], removed = [ Removal [ 1 ] SameRow ] }
                              , Possibles { remaining = Set.fromList [ 3, 4 ], removed = [ Removal [ 1 ] SameRow ] }
                              ]
                            ]
                in
                Expect.equal expected (Solvers.onlyPossibleValueInRow init)
        ]
