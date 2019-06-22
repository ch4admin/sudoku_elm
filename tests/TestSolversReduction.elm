module TestSolversReduction exposing (testSolversReduction)

import Array
import Expect exposing (Expectation)
import Grid exposing (get)
import PossibleList2d exposing (PossibleCell(..), Rationale(..), Removal)
import Set
import SolversReduction
import SudokuGrid
    exposing
        ( SudokuGrid
        )
import Test exposing (..)


testSolversReduction : Test
testSolversReduction =
    describe "Test Solvers Reduction"
        [ test "ValueOnlyPossibleInOneCellInRow" <|
            \_ ->
                let
                    input =
                        [ [ Filled 1
                          , Possibles { remaining = Set.fromList [ 2, 3, 4, 5, 6, 7, 8, 9 ], removed = [ Removal [ 1 ] SameRow ] }
                          , Possibles { remaining = Set.fromList [ 4, 5, 6, 7, 8, 9 ], removed = [ Removal [ 1 ] SameRow ] }
                          , Possibles { remaining = Set.fromList [ 3, 4 ], removed = [ Removal [ 1 ] SameRow ] }
                          ]
                        ]

                    expected =
                        [ [ Filled 1
                          , Possibles { remaining = Set.fromList [ 2 ], removed = [ Removal [ 1 ] SameRow, Removal [ 3, 4, 5, 6, 7, 8, 9 ] ValueOnlyPossibleInOneCellInRow ] }
                          , Possibles { remaining = Set.fromList [ 4, 5, 6, 7, 8, 9 ], removed = [ Removal [ 1 ] SameRow ] }
                          , Possibles { remaining = Set.fromList [ 3, 4 ], removed = [ Removal [ 1 ] SameRow ] }
                          ]
                        ]
                in
                SolversReduction.valueOnlyPossibleInOneCellInRow input |> Expect.equal expected
        ]
