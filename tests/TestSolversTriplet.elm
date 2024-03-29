module TestSolversTriplet exposing (solversTriplet)

import Expect exposing (Expectation)
import Grid exposing (get, getRow)
import PossibleList2d exposing (PossibleCell(..), Rationale(..), Removal)
import PossibleTripletList2d exposing (PossibleTripletCell, PossibleTripletList2d, tripletFromPossibleCells)
import Set exposing (Set)
import SolversTriplet
import Test exposing (..)


solversTriplet : Test
solversTriplet =
    describe "Solvers Triplet"
        [ test "updatePossibleCellsFromTripletCell" <|
            \_ ->
                let
                    possibleCells =
                        [ Possibles { remaining = Set.fromList [ 2, 3, 4 ], removed = [] }
                        , Filled 7
                        , Possibles { remaining = Set.fromList [ 2, 3, 5 ], removed = [] }
                        ]

                    triplet =
                        PossibleTripletCell (Set.fromList [ 7, 2 ]) (Set.fromList [ 3, 4 ])

                    expected =
                        [ Possibles { remaining = Set.fromList [ 2, 3, 4 ], removed = [] }
                        , Filled 7
                        , Possibles { remaining = Set.fromList [ 2, 3 ], removed = [ Removal [ 5 ] BoxRowLogic ] }
                        ]
                in
                SolversTriplet.updatePossibleCellsFromTripletCell triplet possibleCells |> Expect.equal expected
        ]



--        , test "applyBoxRowLogic" <|
--            \_ ->
--                let
--                    input =
--                        Grid.fromList2d
--                            [ [ PossibleTripletCell (Set.fromList [ 9 ]) (Set.fromList [ 3, 4, 5 ])
--                              , PossibleTripletCell (Set.fromList [ 2 ]) (Set.fromList [ 3, 4, 6, 7 ])
--                              , PossibleTripletCell (Set.fromList [ 1 ]) (Set.fromList [ 5, 6, 7 ])
--                              ]
--                            , [ PossibleTripletCell (Set.fromList [ 2, 6, 7 ]) (Set.fromList [])
--                              , PossibleTripletCell (Set.fromList [ 5 ]) (Set.fromList [ 1, 9 ])
--                              , PossibleTripletCell (Set.fromList [ 3, 4, 8 ]) (Set.fromList [])
--                              ]
--                            , [ PossibleTripletCell (Set.fromList [ 8 ]) (Set.fromList [ 1, 3, 4, 5 ])
--                              , PossibleTripletCell (Set.fromList []) (Set.fromList [ 1, 3, 4, 6, 7, 9 ])
--                              , PossibleTripletCell (Set.fromList [ 2 ]) (Set.fromList [ 5, 6, 7, 9 ])
--                              ]
--                            ]
--
--                    expected =
--                        Grid.fromList2d
--                            [ [ PossibleTripletCell (Set.fromList [ 9 ]) (Set.fromList [ 3, 4, 5 ])
--                              , PossibleTripletCell (Set.fromList [ 2 ]) (Set.fromList [ 3, 4, 6, 7 ])
--                              , PossibleTripletCell (Set.fromList [ 1 ]) (Set.fromList [ 5, 6, 7 ])
--                              ]
--                            , [ PossibleTripletCell (Set.fromList [ 2, 6, 7 ]) (Set.fromList [])
--                              , PossibleTripletCell (Set.fromList [ 5 ]) (Set.fromList [ 1, 9 ])
--                              , PossibleTripletCell (Set.fromList [ 3, 4, 8 ]) (Set.fromList [])
--                              ]
--                            , [ PossibleTripletCell (Set.fromList [ 1, 8 ]) (Set.fromList [ 3, 4, 5 ])
--                              , PossibleTripletCell (Set.fromList []) (Set.fromList [ 3, 4, 6, 7 ])
--                              , PossibleTripletCell (Set.fromList [ 2, 9 ]) (Set.fromList [ 5, 6, 7 ])
--                              ]
--                            ]
--                in
--                PossibleTripletList2d.applyBoxRowLogic input |> Expect.equal expected
--        , test "fillWhereOnlyOneCellPossible" <|
--            \_ ->
--                let
--                    input =
--                        [ PossibleTripletCell (Set.fromList [ 9 ]) (Set.fromList [ 3, 4, 5 ])
--                        , PossibleTripletCell (Set.fromList [ 2, 6, 7 ]) (Set.fromList [])
--                        , PossibleTripletCell (Set.fromList [ 8 ]) (Set.fromList [ 1, 3, 4, 5 ])
--                        ]
--
--                    expected =
--                        [ PossibleTripletCell (Set.fromList [ 9 ]) (Set.fromList [ 3, 4, 5 ])
--                        , PossibleTripletCell (Set.fromList [ 2, 6, 7 ]) (Set.fromList [])
--                        , PossibleTripletCell (Set.fromList [ 1, 8 ]) (Set.fromList [ 3, 4, 5 ])
--                        ]
--                in
--                PossibleTripletList2d.fillWhereValueOnlyPossibleInOneCell input |> Expect.equal expected
--        , test "updatePossibleGrid" <|
--            \_ ->
--                let
--                    ptg =
--                        Grid.fromList2d
--                            [ [ PossibleTripletCell (Set.fromList [ 1, 9 ]) (Set.fromList [ 3, 4, 5 ]) ]
--                            ]
--
--                    pg =
--                        Grid.fromList2d
--                            [ [ Filled 1, Possibles { remaining = Set.fromList [ 9, 3, 4 ], removed = [] }, Possibles { remaining = Set.fromList [ 9, 3, 4, 5, 6 ], removed = [] } ]
--                            ]
--
--                    expectedPg =
--                        Grid.fromList2d
--                            [ [ Filled 1, Possibles { remaining = Set.fromList [ 9, 3, 4 ], removed = [] }, Possibles { remaining = Set.fromList [ 9, 3, 4, 5 ], removed = [ Removal [ 6 ] BoxRowLogic ] } ]
--                            ]
--                in
--                PossibleTripletList2d.updatePossibleGridFromRowTriplets ptg pg |> Expect.equal expectedPg
--        , test "columnTripletsFromPossibleGrid" <|
--            \_ ->
--                let
--                    input =
--                        Grid.fromList2d
--                            [ [ Filled 1, Filled 2, Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Possibles { remaining = Set.fromList [ 9 ], removed = [] } ]
--                            , [ Possibles { remaining = Set.fromList [ 2 ], removed = [] }, Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Filled 9, Possibles { remaining = Set.fromList [ 1, 2 ], removed = [] } ]
--                            , [ Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Filled 9, Filled 2, Possibles { remaining = Set.fromList [ 1 ], removed = [] } ]
--                            ]
--
--                    expected =
--                        Grid.fromList2d
--                            [ [ PossibleTripletCell (Set.fromList [ 1, 3 ]) (Set.fromList [ 2 ])
--                              , PossibleTripletCell (Set.fromList [ 2, 3, 4 ]) (Set.fromList [])
--                              , PossibleTripletCell (Set.fromList [ 3, 4, 5 ]) (Set.fromList [])
--                              , PossibleTripletCell (Set.fromList [ 4, 5, 6 ]) (Set.fromList [])
--                              , PossibleTripletCell (Set.fromList [ 5, 6, 7 ]) (Set.fromList [])
--                              , PossibleTripletCell (Set.fromList [ 6, 7, 8 ]) (Set.fromList [])
--                              , PossibleTripletCell (Set.fromList [ 7, 8, 9 ]) (Set.fromList [])
--                              , PossibleTripletCell (Set.fromList [ 8, 9, 2 ]) (Set.fromList [])
--                              , PossibleTripletCell (Set.fromList []) (Set.fromList [ 1, 2, 9 ])
--                              ]
--                            ]
--                in
--                PossibleTripletList2d.columnTripletsFromPossibleGrid input |> Expect.equal expected
