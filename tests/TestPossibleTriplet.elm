module TestPossibleTriplet exposing (possibleTriplet)

import Array
import Expect exposing (Expectation)
import Grid exposing (Coordinate, get, getRow)
import PossibleTripletGrid exposing (PossibleTripletCell, PossibleTripletGrid, tripletFromRow)
import Set exposing (Set)
import SudokuGrid exposing (PossibleCell(..))
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
                Expect.equal (PossibleTripletCell (Set.fromList [ 4 ]) (Set.fromList [ 1, 2, 3, 6, 7 ])) (tripletFromRow input)
        ]
