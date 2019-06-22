module TestPossibleTripletList2d exposing (possibleTriplet)

import Expect exposing (Expectation)
import Grid exposing (get, getRow)
import PossibleList2d exposing (PossibleCell(..), Rationale(..), Removal)
import PossibleTripletList2d exposing (PossibleTripletCell, PossibleTripletList2d, tripletFromPossibleCells)
import Set exposing (Set)
import Test exposing (..)


possibleTriplet : Test
possibleTriplet =
    describe "PossibleTriplet"
        [ test "tripletFromCells" <|
            \_ ->
                let
                    input =
                        [ Filled 4, Possibles { remaining = Set.fromList [ 1, 2, 3 ], removed = [] }, Possibles { remaining = Set.fromList [ 1, 2, 6, 7 ], removed = [] } ]
                in
                PossibleTripletCell (Set.fromList [ 4 ]) (Set.fromList [ 1, 2, 3, 6, 7 ]) |> Expect.equal (tripletFromPossibleCells input)
        , test "rowTripletsFromPossibleList2d" <|
            \_ ->
                let
                    input =
                        [ [ Filled 1, Filled 2, Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Possibles { remaining = Set.fromList [ 9 ], removed = [] } ]
                        , [ Possibles { remaining = Set.fromList [ 2 ], removed = [] }, Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Filled 9, Possibles { remaining = Set.fromList [ 1, 2 ], removed = [] } ]
                        , [ Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Filled 9, Filled 2, Possibles { remaining = Set.fromList [ 1 ], removed = [] } ]
                        ]

                    expected =
                        [ [ PossibleTripletCell (Set.fromList [ 1, 2, 3 ]) (Set.fromList [])
                          , PossibleTripletCell (Set.fromList [ 4, 5, 6 ]) (Set.fromList [])
                          , PossibleTripletCell (Set.fromList [ 7, 8 ]) (Set.fromList [ 9 ])
                          ]
                        , [ PossibleTripletCell (Set.fromList [ 3, 4 ]) (Set.fromList [ 2 ])
                          , PossibleTripletCell (Set.fromList [ 5, 6, 7 ]) (Set.fromList [])
                          , PossibleTripletCell (Set.fromList [ 8, 9 ]) (Set.fromList [ 1, 2 ])
                          ]
                        , [ PossibleTripletCell (Set.fromList [ 3, 4, 5 ]) (Set.fromList [])
                          , PossibleTripletCell (Set.fromList [ 6, 7, 8 ]) (Set.fromList [])
                          , PossibleTripletCell (Set.fromList [ 2, 9 ]) (Set.fromList [ 1 ])
                          ]
                        ]
                in
                PossibleTripletList2d.rowTripletsfromPossibleList2d input |> Expect.equal expected
        , test "columnTripletsFromPossibleList2d" <|
            \_ ->
                let
                    input =
                        [ [ Filled 1, Filled 2, Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Possibles { remaining = Set.fromList [ 9 ], removed = [] } ]
                        , [ Possibles { remaining = Set.fromList [ 2 ], removed = [] }, Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Filled 9, Possibles { remaining = Set.fromList [ 1, 2 ], removed = [] } ]
                        , [ Filled 3, Filled 4, Filled 5, Filled 6, Filled 7, Filled 8, Filled 9, Filled 2, Possibles { remaining = Set.fromList [ 1 ], removed = [] } ]
                        ]

                    expected =
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
                PossibleTripletList2d.columnTripletsFromPossibleList2d input |> Expect.equal expected
        ]
