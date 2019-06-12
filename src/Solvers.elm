module Solvers exposing
    ( actionFromCellWithOnePossible
    , actionFromNoPossibles
    , getActionFromIndexedCells
    , gridFromListofBoxLists
    , removeSameBox
    , removeSameCol
    , removeSameRow
    , solversList
    )

import Array exposing (Array)
import Grid
import Set
import SudokuGrid exposing (Action, PossibleCell(..), PossibleGrid, Rationale(..), Removal)


solversList =
    [ actionFromNoPossibles
    ]


actionFromNoPossibles : PossibleGrid -> Maybe Action
actionFromNoPossibles possiblesGrid =
    getActionFromIndexedCells (Grid.indexedList possiblesGrid)


getActionFromIndexedCells : List ( Int, Int, PossibleCell ) -> Maybe Action
getActionFromIndexedCells list =
    case list of
        [] ->
            Nothing

        ( x, y, cell ) :: ts ->
            let
                acn =
                    actionFromCellWithOnePossible x y cell
            in
            if acn == Nothing then
                getActionFromIndexedCells ts

            else
                acn



--only get action if exactly one possible number
--all other possible value removed


actionFromCellWithOnePossible : Int -> Int -> PossibleCell -> Maybe Action
actionFromCellWithOnePossible x y cell =
    case cell of
        Filled _ ->
            Nothing

        Possibles p ->
            if Set.size p.remaining == 1 then
                let
                    value =
                        List.head (Set.toList p.remaining)
                in
                case value of
                    Nothing ->
                        Nothing

                    Just v ->
                        Just (Action x y v)

            else
                Nothing


removeSameRow : PossibleGrid -> PossibleGrid
removeSameRow possibleGrid =
    Array.fromList (List.map (\arr -> Array.fromList (processGroup SameRow (Array.toList arr))) (Array.toList possibleGrid))


removeSameCol : PossibleGrid -> PossibleGrid
removeSameCol possibleGrid =
    let
        listOfColLists =
            List.map (\arr -> processGroup SameColumn (Array.toList arr)) (Grid.toColumnsList possibleGrid)

        listOfRows =
            Grid.transposeList listOfColLists
    in
    Array.fromList (List.map (\l -> Array.fromList l) listOfRows)


removeSameBox : PossibleGrid -> PossibleGrid
removeSameBox possibleGrid =
    let
        --list of 9 boxes
        boxes =
            SudokuGrid.listOfBoxes possibleGrid

        --list of lists, each list a flattened box
        boxLists =
            List.map (\b -> Grid.list b) boxes

        processed =
            List.map (\l -> processGroup SameBox l) boxLists
    in
    -- reassemble into new box
    Grid.fromList (gridFromListofBoxLists processed [])


processGroup : Rationale -> List PossibleCell -> List PossibleCell
processGroup rationale list =
    let
        filledValues =
            getFilledValues list

        processCell : PossibleCell -> PossibleCell
        processCell cell =
            case cell of
                Filled v ->
                    Filled v

                Possibles p ->
                    let
                        oldSet =
                            p.remaining

                        newSet =
                            Set.diff p.remaining filledValues

                        removedValues =
                            Set.toList (Set.diff oldSet newSet)

                        removed =
                            if List.isEmpty removedValues then
                                p.removed

                            else
                                p.removed ++ [ Removal removedValues rationale ]
                    in
                    Possibles { remaining = newSet, removed = removed }
    in
    List.map processCell list


getFilledValues list =
    Set.fromList
        (List.filterMap
            (\c ->
                case c of
                    Filled v ->
                        Just v

                    _ ->
                        Nothing
            )
            list
        )


gridFromListofBoxLists : List (List a) -> List (List a) -> List (List a)
gridFromListofBoxLists ll output =
    case ll of
        [] ->
            output

        lst ->
            let
                ( first3Lists, remainder ) =
                    splitAt 3 lst
            in
            -- if no values, move on to the next set of 3 lists
            if List.any List.isEmpty first3Lists then
                gridFromListofBoxLists remainder output

            else
                let
                    row =
                        List.concat (List.map (\l -> List.take 3 l) first3Lists)

                    adjLists =
                        List.map (\l -> List.drop 3 l) first3Lists
                in
                gridFromListofBoxLists (adjLists ++ remainder) (output ++ [ row ])


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )
