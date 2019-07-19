module Puzzles exposing (Puzzle, difficultyText, puzzleFromName, puzzles)

import SudokuGrid exposing (SudokuGrid)


type Difficulty
    = Simple
    | Easy
    | Medium
    | Hard


difficultyText difficulty =
    case difficulty of
        Simple ->
            "Simple"

        Easy ->
            "Easy"

        Medium ->
            "Medium"

        Hard ->
            "Hard"


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
        , "...8.9..."
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
    , RawPuzzle
        "8290"
        Medium
        [ "...31..76"
        , "7..5..8.2"
        , "4.....15."
        , ".....2..."
        , ".2..6..47"
        , "9...3...."
        , "..1..7..."
        , "...1..38."
        , "85..2.7.."
        ]
        [ "295318476"
        , "716594832"
        , "438276159"
        , "587942613"
        , "123865947"
        , "964731528"
        , "341687295"
        , "672159384"
        , "859423761"
        ]
    , RawPuzzle
        "5897"
        Hard
        [ "8.....4.."
        , "49.....67"
        , "3..4.691."
        , "5....3.8."
        , "..8...6.."
        , ".6...7..9"
        , ".8.2.1..5"
        , "25.....98"
        , "..3.....6"
        ]
        [ "826179453"
        , "491532867"
        , "375486912"
        , "549623781"
        , "738915624"
        , "162847539"
        , "684291375"
        , "257364198"
        , "913758246"
        ]
    , RawPuzzle
        "6499"
        Hard
        [ "723.16..5"
        , "516...7.."
        , "489.....1"
        , "...4..2.3"
        , "8...2.54."
        , "...73...."
        , "67..4...."
        , "......9.."
        , ".9.1.3..."
        ]
        [ "723816495"
        , "516394728"
        , "489572631"
        , "165489273"
        , "837621549"
        , "942735186"
        , "671948352"
        , "358267914"
        , "294153867"
        ]
    ]
