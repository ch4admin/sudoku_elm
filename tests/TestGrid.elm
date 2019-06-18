module TestGrid exposing (grid)

import Array
import Expect exposing (Expectation)
import Grid exposing (Coordinate, get, getRow)
import Test exposing (..)


grid : Test
grid =
    describe "Grid"
        [ test "coordinate init" <|
            \_ ->
                Expect.equal (Coordinate 1 2) { x = 1, y = 2 }
        , test "fromList" <|
            \_ ->
                Expect.equal (Grid.fromList [ [ 1, 2 ], [ 3, 4 ] ]) (Array.fromList [ Array.fromList [ 1, 2 ], Array.fromList [ 3, 4 ] ])
        , test "initSquare" <|
            \_ ->
                Expect.equal (Grid.fromList [ [ 3, 3 ], [ 3, 3 ] ]) (Grid.initSquare 2 3)
        , test "getRow" <|
            \_ ->
                let
                    g =
                        Grid.fromList [ [ 1, 1 ], [ 2, 2 ] ]
                in
                Expect.equal (getRow 1 g) (Just (Array.fromList [ 2, 2 ]))
        , test "get" <|
            \_ ->
                let
                    g =
                        Grid.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (get 1 1 g) (Just 4)
        , test "set" <|
            \_ ->
                let
                    g =
                        Grid.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (Grid.fromList [ [ 1, 2 ], [ 100, 4 ] ]) (Grid.set 1 0 100 g)
        , test "array" <|
            \_ ->
                Expect.equal (Array.get 1 (Array.fromList [ Nothing, Just 2 ])) (Just (Just 2))
        , test "map" <|
            \_ ->
                Expect.equal (Grid.map (\v -> v + 1) (Grid.fromList [ [ 1, 2 ], [ 3, 4 ] ])) (Grid.fromList [ [ 2, 3 ], [ 4, 5 ] ])
        , test "indexedList" <|
            \_ ->
                let
                    g =
                        Grid.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (Grid.indexedList g) [ ( 0, 0, 1 ), ( 0, 1, 2 ), ( 1, 0, 3 ), ( 1, 1, 4 ) ]
        , test "getColumn" <|
            \_ ->
                let
                    g =
                        Grid.fromList [ [ 1, 2 ], [ 3, 4 ] ]
                in
                Expect.equal (Just (Array.fromList [ 2, 4 ])) (Grid.getColumn 1 g)
        , test "transposeList" <|
            \_ ->
                Expect.equal (Grid.transposeList [ [ 1, 2 ], [ 3, 4 ] ]) [ [ 1, 3 ], [ 2, 4 ] ]
        , test "sliceGrid" <|
            \_ ->
                let
                    inp =
                        Grid.fromList [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

                    expected =
                        Grid.fromList [ [ 2, 3 ], [ 5, 6 ] ]
                in
                Expect.equal expected (Grid.slice 0 2 1 3 inp)
        , test "toListOfBoxLists" <|
            \_ ->
                let
                    inp =
                        Grid.fromList [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

                    expected =
                        [ [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] ]
                in
                Grid.toListOfBoxLists 3 3 inp |> Expect.equal expected
        , test "toListOfBoxLists 3" <|
            \_ ->
                let
                    inp =
                        Grid.fromList [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

                    expected =
                        [ [ 1, 4, 7 ], [ 2, 5, 8 ], [ 3, 6, 9 ] ]
                in
                Grid.toListOfBoxLists 3 1 inp |> Expect.equal expected
        , test "fromListOfBoxLists 3" <|
            \_ ->
                let
                    inp =
                        [ [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
                        , [ 11, 12, 13, 14, 15, 16, 17, 18, 19 ]
                        , [ 21, 22, 23, 24, 25, 26, 27, 28, 29 ]
                        ]

                    expected =
                        Grid.fromList [ [ 1, 2, 3, 11, 12, 13, 21, 22, 23 ], [ 4, 5, 6, 14, 15, 16, 24, 25, 26 ], [ 7, 8, 9, 17, 18, 19, 27, 28, 29 ] ]
                in
                Grid.fromListOfBoxLists 3 3 inp |> Expect.equal expected
        ]
