module SolversReduction exposing (onlyPossibleValueInGroup, valueOnlyPossibleInOneCellInBox, valueOnlyPossibleInOneCellInColumn, valueOnlyPossibleInOneCellInRow)

import Dict
import Grid
import List2d
import ListExtra
import PossibleList2d exposing (PossibleCell(..), PossibleList2d, Rationale(..), Removal)
import Set


valueOnlyPossibleInOneCellInRow : PossibleList2d -> PossibleList2d
valueOnlyPossibleInOneCellInRow p2d =
    List.map (\row -> onlyPossibleValueInGroup ValueOnlyPossibleInOneCellInRow row) p2d


valueOnlyPossibleInOneCellInColumn : PossibleList2d -> PossibleList2d
valueOnlyPossibleInOneCellInColumn p2d =
    p2d |> List2d.transpose |> List.map (\col -> onlyPossibleValueInGroup ValueOnlyPossibleInOneCellInColumn col) |> List2d.transpose


valueOnlyPossibleInOneCellInBox : PossibleList2d -> PossibleList2d
valueOnlyPossibleInOneCellInBox p2d =
    let
        --list of 9 boxes
        boxLists =
            List2d.toListOfBoxLists 3 3 p2d

        processed =
            List.map (\box -> onlyPossibleValueInGroup ValueOnlyPossibleInOneCellInBox box) boxLists
    in
    -- reassemble into new box
    List2d.fromListOfBoxLists 3 3 processed


onlyPossibleValueInGroup : Rationale -> List PossibleCell -> List PossibleCell
onlyPossibleValueInGroup rationale pCells =
    let
        -- aggregate all possible values for group
        possibleValues =
            getPossibleValuesFromList pCells

        -- get counts of each value in a dict ex: {1: 4, 2: 1}
        counts =
            ListExtra.frequencies possibleValues

        -- filter down to values with counts of 1
        -- this means this value is only possible in 1 cell
        uniquePossibleValues =
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
                Filled v ->
                    Filled v

                Possibles p ->
                    let
                        intersection =
                            Set.intersect uniquePossibleValues p.remaining
                    in
                    if Set.isEmpty intersection then
                        Possibles p

                    else
                        let
                            -- possible in multiple sweeps that we run the same logic twice
                            removedValues =
                                Set.toList (Set.diff p.remaining intersection)
                        in
                        Possibles
                            { remaining = intersection
                            , removed =
                                p.removed
                                    ++ (if List.isEmpty removedValues then
                                            []

                                        else
                                            [ Removal removedValues rationale ]
                                       )
                            }
    in
    List.map processCell pCells


getPossibleValuesFromList : List PossibleCell -> List Int
getPossibleValuesFromList listOfpCells =
    let
        getPossibleValues c =
            case c of
                Possibles p ->
                    Set.toList p.remaining

                _ ->
                    []
    in
    List.map getPossibleValues listOfpCells |> List.concat
