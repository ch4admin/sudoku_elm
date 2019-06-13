module Puzzles exposing (Puzzle, puzzleFromName, puzzles)

import SudokuGrid exposing (SudokuGrid)


type Difficulty
    = Simple
    | Easy
    | Medium
    | Hard


type alias Puzzle =
    { name : String, difficulty : Difficulty, puzzle : SudokuGrid, solution : SudokuGrid }


type alias RawPuzzle =
    { name : String, difficulty : Difficulty, puzzle : List String, solution : List String }


puzzleFromName : String -> Maybe Puzzle
puzzleFromName name =
    List.head (List.filter (\m -> m.name == name) puzzles)


puzzles =
    List.map puzzleFromRaw rawPuzzles


puzzleFromRaw : RawPuzzle -> Puzzle
puzzleFromRaw r =
    Puzzle r.name r.difficulty (SudokuGrid.fromListOfString r.puzzle) (SudokuGrid.fromListOfString r.solution)


rawPuzzles =
    [ RawPuzzle
        "2573"
        Simple
        [ "978..6..."
        , ".5..4...."
        , "4....85.."
        , "....32.1."
        , "3.7.5.6.2"
        , ".1.67...."
        , "..32....8"
        , "....8..3."
        , "...4..765"
        ]
        [ "978516324"
        , "152347986"
        , "436928571"
        , "569832417"
        , "387154692"
        , "214679853"
        , "793265148"
        , "645781239"
        , "821493765"
        ]
    , RawPuzzle
        "4291"
        Easy
        [ "....97..."
        , "...3.2.7."
        , "..2..64.."
        , "..9...1.."
        , ".67.5.34."
        , "..8...2.."
        , "..42..8.."
        , "...8.8..."
        , ".3.46...."
        ]
        [ "413597682"
        , "685342971"
        , "972186453"
        , "349728165"
        , "267951348"
        , "158634297"
        , "594273816"
        , "726819534"
        , "831465729"
        ]
    ]
