module Solvers exposing
    ( actionFromCellWithOnePossible
    , actionFromNoPossibles
    , getActionFromIndexedCells
    , gridFromListofBoxLists
    , onlyPossibleValueInRow
    , removeSameBox
    , removeSameCol
    , removeSameRow
    , solversList
    )

import Array exposing (Array)
import Dict
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
                        Just (Action x y v p.removed)

            else
                Nothing


removeSameRow : PossibleGrid -> PossibleGrid
removeSameRow possibleGrid =
    Array.fromList (List.map (\arr -> Array.fromList (removeFilledValuesFromPossiblesForGroup SameRow (Array.toList arr))) (Array.toList possibleGrid))


onlyPossibleValueInRow : PossibleGrid -> PossibleGrid
onlyPossibleValueInRow possibleGrid =
    Array.fromList (List.map (\arr -> Array.fromList (onlyPossibleValueInGroup ValueOnlyPossibleInOneCellInRow (Array.toList arr))) (Array.toList possibleGrid))


removeSameCol : PossibleGrid -> PossibleGrid
removeSameCol possibleGrid =
    let
        listOfColLists =
            List.map (\arr -> removeFilledValuesFromPossiblesForGroup SameColumn (Array.toList arr)) (Grid.toColumnsList possibleGrid)

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
            List.map (\l -> removeFilledValuesFromPossiblesForGroup SameBox l) boxLists
    in
    -- reassemble into new box
    Grid.fromList (gridFromListofBoxLists processed [])


onlyPossibleValueInGroup : Rationale -> List PossibleCell -> List PossibleCell
onlyPossibleValueInGroup rationale list =
    let
        getPossibleValues c =
            case c of
                Possibles p ->
                    Set.toList p.remaining

                _ ->
                    []

        possibleValues =
            List.concat (List.map getPossibleValues list)

        counts =
            frequencies possibleValues

        onlyOnePossibleValueSet =
            Set.fromList
                (List.filterMap
                    (\( k, v ) ->
                        if v == 1 then
                            Just k

                        else
                            Nothing
                    )
                    (Dict.toList counts)
                )

        processCell cell =
            case cell of
                Possibles p ->
                    let
                        intersection =
                            Set.intersect onlyOnePossibleValueSet p.remaining
                    in
                    if Set.isEmpty intersection then
                        Possibles p

                    else
                        let
                            -- possible in multiple sweeps that we run the same logic twice
                            removedValues =
                                Set.toList (Set.diff p.remaining intersection)
                        in
                        Debug.log "Possibles"
                            (Possibles
                                { remaining = intersection
                                , removed =
                                    p.removed
                                        ++ (if List.isEmpty removedValues then
                                                []

                                            else
                                                [ Removal removedValues rationale ]
                                           )
                                }
                            )

                Filled v ->
                    Filled v
    in
    List.map processCell list


removeFilledValuesFromPossiblesForGroup : Rationale -> List PossibleCell -> List PossibleCell
removeFilledValuesFromPossiblesForGroup rationale list =
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


frequencies : List comparable -> Dict.Dict comparable Int
frequencies list =
    list
        |> List.foldl
            (\el counter ->
                Dict.get el counter
                    |> Maybe.withDefault 0
                    |> (\count -> count + 1)
                    |> (\count -> Dict.insert el count counter)
            )
            Dict.empty
