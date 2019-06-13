module SudokuGrid exposing
    ( Action
    , PossibleCell(..)
    , PossibleGrid
    , Rationale(..)
    , Removal
    , SudokuGrid
    , boxFromList
    , fromList
    , fromListOfString
    , fromPossibleGrid
    , initEmpty
    , initPossibleGrid
    , isSolved
    , listOfBoxes
    , possibleCellFromValue
    , updatePossibleGrid
    )

import Array exposing (Array)
import Grid
import Set


type alias SudokuGrid =
    Grid.Grid (Maybe Int)


fromList : List (List (Maybe Int)) -> SudokuGrid
fromList aa =
    Grid.fromList aa


fromListOfString : List String -> SudokuGrid
fromListOfString ls =
    Array.fromList (List.map arrFromString ls)


arrFromString : String -> Array (Maybe Int)
arrFromString s =
    Array.fromList (List.map intFromChar (String.toList s))


intFromChar c =
    String.toInt (String.fromChar c)


type PossibleCell
    = Filled Int
    | Possibles { remaining : Set.Set Int, removed : List Removal }


type alias Removal =
    { values : List Int, rationale : Rationale }


type Rationale
    = SameRow
    | SameColumn
    | SameBox
    | ValueOnlyPossibleInOneCellInRow
    | ValueOnlyPossibleInOneCellInColumn
    | ValueOnlyPossibleInOneCellInBox


type alias Action =
    { x : Int, y : Int, value : Int, removed : List Removal }


type alias PossibleGrid =
    Grid.Grid PossibleCell


initPossibleGrid : SudokuGrid -> PossibleGrid
initPossibleGrid grid =
    Grid.map possibleCellFromValue grid



--PossibleCell from Sudoku value (Nothing or filled value)


isSolved : PossibleGrid -> Bool
isSolved possibleGrid =
    List.length (List.filter isPossible (Grid.list possibleGrid)) == 0


isPossible : PossibleCell -> Bool
isPossible c =
    case c of
        Possibles _ ->
            True

        _ ->
            False



--create a possible cell from a sudoku value or nothing


possibleCellFromValue : Maybe Int -> PossibleCell
possibleCellFromValue v =
    case v of
        Nothing ->
            Possibles { remaining = Set.fromList (List.range 1 9), removed = [] }

        Just value ->
            Filled value



--find a solution for the grid
-- apply action to possibleGrid to change it


updatePossibleGrid action possibleGrid =
    Grid.set action.x action.y (Filled action.value) possibleGrid


initEmpty : SudokuGrid
initEmpty =
    Grid.initSquare 9 Nothing


listOfBoxes : Grid.Grid a -> List (Grid.Grid a)
listOfBoxes grid =
    let
        boxWidth =
            Array.length grid // 3

        getBoxesFromRow : Int -> Grid.Grid a -> List (Grid.Grid a)
        getBoxesFromRow boxRow sg =
            List.map (\c -> getBox boxRow c sg) (List.range 0 (boxWidth - 1))

        l =
            List.map (\r -> getBoxesFromRow r grid) (List.range 0 (boxWidth - 1))
    in
    List.concat l



-- get box specifying boardrow and boardcol


getBox : Int -> Int -> Grid.Grid a -> Grid.Grid a
getBox boxRow boxCol g =
    let
        baseRow =
            boxRow * 3

        baseCol =
            boxCol * 3
    in
    Grid.slice baseRow (baseRow + 3) baseCol (baseCol + 3) g



-- box from a list of 9 values


boxFromList : List a -> Grid.Grid a
boxFromList l =
    Grid.fromList (split 3 l)


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)


fromPossibleGrid : PossibleGrid -> SudokuGrid
fromPossibleGrid pg =
    let
        cellValue c =
            case c of
                Filled v ->
                    Just v

                Possibles p ->
                    Nothing
    in
    Grid.map cellValue pg
