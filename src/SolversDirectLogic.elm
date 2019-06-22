module SolversDirectLogic exposing (removeFilledInSameBox, removeFilledInSameColumn, removeFilledInSameRow, removeFilledValuesFromPossiblesInList)

import Dict
import Grid
import List2d
import ListExtra
import PossibleList2d exposing (PossibleCell(..), PossibleList2d, Rationale(..), Removal)
import Set


removeFilledInSameRow : PossibleList2d -> PossibleList2d
removeFilledInSameRow p2d =
    List.map (\row -> removeFilledValuesFromPossiblesInList SameRow row) p2d


removeFilledInSameColumn : PossibleList2d -> PossibleList2d
removeFilledInSameColumn p2d =
    p2d |> List2d.transpose |> List.map (\col -> removeFilledValuesFromPossiblesInList SameColumn col) |> List2d.transpose


removeFilledInSameBox : PossibleList2d -> PossibleList2d
removeFilledInSameBox p2d =
    let
        --list of 9 boxes
        boxLists =
            List2d.toListOfBoxLists 3 3 p2d

        processed =
            List.map (\box -> removeFilledValuesFromPossiblesInList SameBox box) boxLists
    in
    -- reassemble into new box
    List2d.fromListOfBoxLists 3 3 processed


{-| Take any filled values in the group (a row, a column or a box), and remove these from the possible values in any other cell
-}
removeFilledValuesFromPossiblesInList : Rationale -> List PossibleCell -> List PossibleCell
removeFilledValuesFromPossiblesInList rationale list =
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
