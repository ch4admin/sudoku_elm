module TestPossibleList2d exposing (testPossibleList2d)

import Array
import Expect exposing (Expectation)
import List2d
import PossibleList2d exposing (PossibleCell(..), PossibleList2d)
import Set
import Test exposing (..)


testPossibleList2d : Test
testPossibleList2d =
    describe "p2d"
        [ test "isValid" <|
            \_ ->
                let
                    input =
                        [ [ Filled 1, Possibles { remaining = Set.fromList [ 2, 6 ], removed = [] } ], [ Filled 3, Filled 4 ] ]
                in
                PossibleList2d.isValid input |> Expect.equal True
        , test "invalid - duplicate" <|
            \_ ->
                let
                    input =
                        [ [ Filled 1, Filled 2 ], [ Filled 3, Filled 3 ] ]
                in
                PossibleList2d.isValid input |> Expect.equal False
        , test "invalid - Possible with no options" <|
            \_ ->
                let
                    input =
                        [ [ Filled 1, Possibles { remaining = Set.fromList [], removed = [] } ], [ Filled 3, Filled 3 ] ]
                in
                PossibleList2d.isValid input |> Expect.equal False
        , test "invalid col" <|
            \_ ->
                let
                    input =
                        [ [ Filled 1, Filled 2 ], [ Filled 1, Filled 3 ] ]
                in
                PossibleList2d.isValid input |> Expect.equal False
        , test "invalid box" <|
            \_ ->
                let
                    input =
                        [ [ Filled 1, Filled 2, Filled 3 ], [ Filled 4, Filled 5, Filled 6 ], [ Filled 7, Filled 8, Filled 2 ] ]
                in
                PossibleList2d.isValid input |> Expect.equal False
        , test "isValidRow" <|
            \_ ->
                let
                    input =
                        [ Filled 1, Filled 2 ]
                in
                PossibleList2d.isValidRowColOrBox input |> Expect.equal True
        ]
