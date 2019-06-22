module Action exposing (Action, getAction, getActionFromCell, updatePossibleList2d)

import List2d
import PossibleList2d exposing (PossibleCell(..), PossibleList2d, Removal)
import Set


type alias Action =
    { x : Int, y : Int, value : Int, removed : List Removal }


{-| Apply action to update possibleList2d
-}
updatePossibleList2d : Action -> PossibleList2d -> PossibleList2d
updatePossibleList2d action p2d =
    List2d.set action.x action.y (Filled action.value) p2d


getAction : PossibleList2d -> Maybe Action
getAction p2d =
    getActionFromIndexedCells (List2d.indexedList p2d)


getActionFromIndexedCells : List ( Int, Int, PossibleCell ) -> Maybe Action
getActionFromIndexedCells list =
    case list of
        [] ->
            Nothing

        ( x, y, cell ) :: ts ->
            let
                action =
                    getActionFromCell x y cell
            in
            if action == Nothing then
                getActionFromIndexedCells ts

            else
                action


{-| only get action if exactly one possible number
all other possible value removed
-}
getActionFromCell : Int -> Int -> PossibleCell -> Maybe Action
getActionFromCell x y cell =
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
