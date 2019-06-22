module SudokuGrid exposing
    ( SudokuGrid
    , boxFromList
    , fromList
    , fromListOfString
    , initEmpty
    , listOfBoxes
    )

import Array exposing (Array)
import Grid
import Set


type alias SudokuGrid =
    Grid.Grid (Maybe Int)


fromList : List (List (Maybe Int)) -> SudokuGrid
fromList aa =
    Grid.fromList2d aa


fromListOfString : List String -> SudokuGrid
fromListOfString ls =
    Array.fromList (List.map arrFromString ls)


arrFromString : String -> Array (Maybe Int)
arrFromString s =
    Array.fromList (List.map intFromChar (String.toList s))


intFromChar c =
    String.toInt (String.fromChar c)



--find a solution for the grid
-- apply action to possibleGrid to change it


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
    Grid.fromList2d (split 3 l)


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)
