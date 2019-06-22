module PossibleList2d exposing
    ( PossibleCell(..)
    , PossibleList2d
    , Rationale(..)
    , Removal
    , fromSudokuGrid
    , isSolved
    , possibleCellFromValue
    , toSudokuGrid
    )

import Grid
import List2d exposing (List2d)
import Set exposing (Set)
import SudokuGrid exposing (SudokuGrid)


type alias PossibleList2d =
    List2d PossibleCell


{-| PossibleCell from Sudoku value (Nothing or filled value)
-}
type PossibleCell
    = Filled Int
    | Possibles { remaining : Set Int, removed : List Removal }


type alias Removal =
    { values : List Int, rationale : Rationale }


type Rationale
    = SameRow
    | SameColumn
    | SameBox
    | ValueOnlyPossibleInOneCellInRow
    | ValueOnlyPossibleInOneCellInColumn
    | ValueOnlyPossibleInOneCellInBox
    | BoxRowLogic


fromSudokuGrid : SudokuGrid -> PossibleList2d
fromSudokuGrid sudokuGrid =
    Grid.toListOfList sudokuGrid |> List2d.map possibleCellFromValue


toSudokuGrid : PossibleList2d -> SudokuGrid
toSudokuGrid p2d =
    let
        cellValue c =
            case c of
                Filled v ->
                    Just v

                Possibles p ->
                    Nothing
    in
    SudokuGrid.fromList (List2d.map cellValue p2d)


{-| create a possible cell from a sudoku value. If a value, its a filled, if nothing its a Possibles
-}
possibleCellFromValue : Maybe Int -> PossibleCell
possibleCellFromValue v =
    case v of
        Nothing ->
            Possibles { remaining = Set.fromList (List.range 1 9), removed = [] }

        Just value ->
            Filled value


isSolved : PossibleList2d -> Bool
isSolved p2d =
    let
        cells =
            List.concat p2d
    in
    not (List.any isPossibles cells)


isPossibles : PossibleCell -> Bool
isPossibles c =
    case c of
        Possibles _ ->
            True

        _ ->
            False
