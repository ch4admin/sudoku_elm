module PossibleList2d exposing
    ( PossibleCell(..)
    , PossibleList2d
    , Rationale(..)
    , Removal
    , fromSudokuGrid
    , getFilledValues
    , isSolved
    , isValid
    , isValidRowColOrBox
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
    | ExhaustiveEnumerationInvalid


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


isValid p2d =
    if List.any (\row -> not (isValidRowColOrBox row)) p2d then
        False

    else if List.any (\col -> not (isValidRowColOrBox col)) (List2d.transpose p2d) then
        False

    else if List.any (\box -> not (isValidRowColOrBox box)) (List2d.toListOfBoxLists 3 3 p2d) then
        False

    else
        True


isValidRowColOrBox : List PossibleCell -> Bool
isValidRowColOrBox list =
    let
        filledValues =
            getFilledValues list

        filledLength =
            List.length filledValues
    in
    -- duplicate fill values in a group is an error
    if filledLength == 0 then
        True

    else if Set.size (Set.fromList filledValues) /= filledLength then
        False

    else
        List.all (\c -> isValidCell c) list


getFilledValues : List PossibleCell -> List Int
getFilledValues list =
    List.filterMap
        (\c ->
            case c of
                Filled v ->
                    Just v

                _ ->
                    Nothing
        )
        list


isValidCell : PossibleCell -> Bool
isValidCell c =
    case c of
        Filled _ ->
            True

        Possibles p ->
            Set.size p.remaining > 0
