module TestList2d exposing (testList2d)

import Array
import Expect exposing (Expectation)
import List2d
import Test exposing (..)


testList2d : Test
testList2d =
    describe "List2d"
        [ test "set" <|
            \_ ->
                let
                    input =
                        [ [ 1, 2 ], [ 3, 4 ] ]

                    expected =
                        [ [ 1, 2 ], [ 5, 4 ] ]
                in
                List2d.set 1 0 5 input |> Expect.equal expected
        , test "get" <|
            \_ ->
                List2d.get 1 1 [ [ 1, 2 ], [ 3, 4 ] ] |> Expect.equal (Just 4)
        , test "transpose" <|
            \_ ->
                Expect.equal (List2d.transpose [ [ 1, 2 ], [ 3, 4 ] ]) [ [ 1, 3 ], [ 2, 4 ] ]
        , test "toListOfBoxLists" <|
            \_ ->
                let
                    inp =
                        [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

                    expected =
                        [ [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] ]
                in
                List2d.toListOfBoxLists 3 3 inp |> Expect.equal expected
        , test "toListOfBoxLists 3" <|
            \_ ->
                let
                    inp =
                        [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

                    expected =
                        [ [ 1, 4, 7 ], [ 2, 5, 8 ], [ 3, 6, 9 ] ]
                in
                List2d.toListOfBoxLists 3 1 inp |> Expect.equal expected
        , test "fromListOfBoxLists 3" <|
            \_ ->
                let
                    inp =
                        [ [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
                        , [ 11, 12, 13, 14, 15, 16, 17, 18, 19 ]
                        , [ 21, 22, 23, 24, 25, 26, 27, 28, 29 ]
                        ]

                    expected =
                        [ [ 1, 2, 3, 11, 12, 13, 21, 22, 23 ], [ 4, 5, 6, 14, 15, 16, 24, 25, 26 ], [ 7, 8, 9, 17, 18, 19, 27, 28, 29 ] ]
                in
                List2d.fromListOfBoxLists 3 3 inp |> Expect.equal expected
        ]
