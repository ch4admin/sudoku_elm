module TestSudoku exposing (initGrid)

import Array
import Expect exposing (Expectation)
import Grid exposing (Coordinate, get)
import Set
import SudokuGrid
    exposing
        ( Action
        , PossibleCell(..)
        , PossibleGrid
        , SudokuGrid
        , fromList
        , initPossibleGrid
        , possibleCellFromValue
        )
import Test exposing (..)


simpleGrid =
    fromList
        [ [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 7, Nothing, Just 3 ]
        , [ Nothing, Nothing, Nothing, Just 9, Nothing, Nothing, Nothing, Just 1, Nothing ]
        , [ Nothing, Just 7, Just 4, Nothing, Just 5, Nothing, Nothing, Just 9, Nothing ]
        , [ Just 8, Nothing, Nothing, Just 7, Nothing, Just 4, Just 6, Just 3, Nothing ]
        , [ Just 7, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 9 ]
        , [ Nothing, Just 5, Just 9, Just 8, Nothing, Just 2, Nothing, Nothing, Just 1 ]
        , [ Nothing, Just 8, Nothing, Nothing, Just 6, Nothing, Just 9, Just 4, Nothing ]
        , [ Nothing, Just 2, Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing ]
        , [ Just 4, Nothing, Just 7, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
        ]


initGrid : Test
initGrid =
    describe "Init SudokuGrid"
        [ test "init simple" <|
            \_ ->
                Expect.equal (get 0 6 simpleGrid) (Just (Just 7))
        , test "cellFromValue 7" <|
            \_ ->
                Expect.equal (possibleCellFromValue (Just 7)) (Filled 7)
        , test "cellFromValue nothing" <|
            \_ ->
                Expect.equal (possibleCellFromValue Nothing) (Possibles { remaining = Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], removed = [] })
        , test "init Possibles" <|
            \_ ->
                Expect.equal (initPossibleGrid (SudokuGrid.fromList [ [ Nothing, Just 1 ], [ Just 4, Nothing ] ]))
                    (Grid.fromList
                        [ [ Possibles { remaining = Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], removed = [] }, Filled 1 ]
                        , [ Filled 4, Possibles { remaining = Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], removed = [] } ]
                        ]
                    )
        , test "fromListOfString" <|
            \_ ->
                let
                    expected =
                        SudokuGrid.fromList [ [ Nothing, Just 1 ], [ Just 4, Nothing ] ]
                in
                Expect.equal expected (SudokuGrid.fromListOfString [ ".1", "4." ])
        ]
