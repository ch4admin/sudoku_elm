module TestPossibleTriplet exposing (possibleTriplet)

import Array
import Expect exposing (Expectation)
import Grid exposing (get, getRow)
import PossibleTripletGrid exposing (PossibleTripletCell, PossibleTripletGrid, tripletFromPossibleCells)
import Set exposing (Set)
import SudokuGrid exposing (PossibleCell(..), Rationale(..), Removal)
import Test exposing (..)


possibleTriplet : Test
possibleTriplet =
    describe "PossibleTriplet"
        [ test "tripletFromRow" <|
            \_ ->
                let
                    input =
                        [ Filled 4, Possibles { remaining = Set.fromList [ 1, 2, 3 ], removed = [] }, Possibles { remaining = Set.fromList [ 1, 2, 6, 7 ], removed = [] } ]
                in
                Expect.equal (PossibleTripletCell (Set.fromList [ 4 ]) (Set.fromList [ 1, 2, 3, 6, 7 ])) (tripletFromPossibleCells input)
        , test "applyBoxRowLogic" <|
            \_ ->
                let
                    input =
                        Grid.fromList
                            [ [ PossibleTripletCell (Set.fromList [ 9 ]) (Set.fromList [ 3, 4, 5 ])
                              , PossibleTripletCell (Set.fromList [ 2 ]) (Set.fromList [ 3, 4, 6, 7 ])
                              , PossibleTripletCell (Set.fromList [ 1 ]) (Set.fromList [ 5, 6, 7 ])
                              ]
                            , [ PossibleTripletCell (Set.fromList [ 2, 6, 7 ]) (Set.fromList [])
                              , PossibleTripletCell (Set.fromList [ 5 ]) (Set.fromList [ 1, 9 ])
                              , PossibleTripletCell (Set.fromList [ 3, 4, 8 ]) (Set.fromList [])
                              ]
                            , [ PossibleTripletCell (Set.fromList [ 8 ]) (Set.fromList [ 1, 3, 4, 5 ])
                              , PossibleTripletCell (Set.fromList []) (Set.fromList [ 1, 3, 4, 6, 7, 9 ])
                              , PossibleTripletCell (Set.fromList [ 2 ]) (Set.fromList [ 5, 6, 7, 9 ])
                              ]
                            ]

                    expected =
                        Grid.fromList
                            [ [ PossibleTripletCell (Set.fromList [ 9 ]) (Set.fromList [ 3, 4, 5 ])
                              , PossibleTripletCell (Set.fromList [ 2 ]) (Set.fromList [ 3, 4, 6, 7 ])
                              , PossibleTripletCell (Set.fromList [ 1 ]) (Set.fromList [ 5, 6, 7 ])
                              ]
                            , [ PossibleTripletCell (Set.fromList [ 2, 6, 7 ]) (Set.fromList [])
                              , PossibleTripletCell (Set.fromList [ 5 ]) (Set.fromList [ 1, 9 ])
                              , PossibleTripletCell (Set.fromList [ 3, 4, 8 ]) (Set.fromList [])
                              ]
                            , [ PossibleTripletCell (Set.fromList [ 1, 8 ]) (Set.fromList [ 3, 4, 5 ])
                              , PossibleTripletCell (Set.fromList []) (Set.fromList [ 3, 4, 6, 7 ])
                              , PossibleTripletCell (Set.fromList [ 2, 9 ]) (Set.fromList [ 5, 6, 7 ])
                              ]
                            ]
                in
                PossibleTripletGrid.applyBoxRowLogic input |> Expect.equal expected
        , test "fillWhereOnlyOneCellPossible" <|
            \_ ->
                let
                    input =
                        [ PossibleTripletCell (Set.fromList [ 9 ]) (Set.fromList [ 3, 4, 5 ])
                        , PossibleTripletCell (Set.fromList [ 2, 6, 7 ]) (Set.fromList [])
                        , PossibleTripletCell (Set.fromList [ 8 ]) (Set.fromList [ 1, 3, 4, 5 ])
                        ]

                    expected =
                        [ PossibleTripletCell (Set.fromList [ 9 ]) (Set.fromList [ 3, 4, 5 ])
                        , PossibleTripletCell (Set.fromList [ 2, 6, 7 ]) (Set.fromList [])
                        , PossibleTripletCell (Set.fromList [ 1, 8 ]) (Set.fromList [ 3, 4, 5 ])
                        ]
                in
                PossibleTripletGrid.fillWhereValueOnlyPossibleInOneCell input |> Expect.equal expected
        , test "updatePossibleGrid" <|
            \_ ->
                let
                    ptg =
                        Grid.fromList
                            [ [ PossibleTripletCell (Set.fromList [ 1, 9 ]) (Set.fromList [ 3, 4, 5 ]) ]
                            ]

                    pg =
                        Grid.fromList
                            [ [ Filled 1, Possibles { remaining = Set.fromList [ 9, 3, 4 ], removed = [] }, Possibles { remaining = Set.fromList [ 9, 3, 4, 5, 6 ], removed = [] } ]
                            ]

                    expectedPg =
                        Grid.fromList
                            [ [ Filled 1, Possibles { remaining = Set.fromList [ 9, 3, 4 ], removed = [] }, Possibles { remaining = Set.fromList [ 9, 3, 4, 5 ], removed = [ Removal [ 6 ] BoxRowLogic ] } ]
                            ]
                in
                PossibleTripletGrid.updatePossibleGridFromRowTriplets ptg pg |> Expect.equal expectedPg
        , test "columnTripletsFromPossibleGrid" <|
            \_ ->
                let
                    input =
                        Grid.fromList
                            [ [ Filled 1, Filled 2, Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Possibles { remaining = Set.fromList [ 9 ], removed = [] } ]
                            , [ Possibles { remaining = Set.fromList [ 2 ], removed = [] }, Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Filled 9, Possibles { remaining = Set.fromList [ 1, 2 ], removed = [] } ]
                            , [ Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Filled 9, Filled 2, Possibles { remaining = Set.fromList [ 1 ], removed = [] } ]
                            ]

                    expected =
                        Grid.fromList
                            [ [ PossibleTripletCell (Set.fromList [ 1, 3 ]) (Set.fromList [ 2 ])
                              , PossibleTripletCell (Set.fromList [ 2, 3, 4 ]) (Set.fromList [])
                              , PossibleTripletCell (Set.fromList [ 3, 4, 5 ]) (Set.fromList [])
                              , PossibleTripletCell (Set.fromList [ 4, 5, 6 ]) (Set.fromList [])
                              , PossibleTripletCell (Set.fromList [ 5, 6, 7 ]) (Set.fromList [])
                              , PossibleTripletCell (Set.fromList [ 6, 7, 8 ]) (Set.fromList [])
                              , PossibleTripletCell (Set.fromList [ 7, 8, 9 ]) (Set.fromList [])
                              , PossibleTripletCell (Set.fromList [ 8, 9, 2 ]) (Set.fromList [])
                              , PossibleTripletCell (Set.fromList []) (Set.fromList [ 1, 2, 9 ])
                              ]
                            ]
                in
                PossibleTripletGrid.columnTripletsFromPossibleGrid input |> Expect.equal expected
        ]
