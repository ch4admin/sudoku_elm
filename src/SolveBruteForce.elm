module SolveBruteForce exposing (bruteForceRemoveInvalid, getActionsFromIndexedCell, getPossibleActions)

import Action exposing (Action)
import List2d
import PossibleList2d exposing (PossibleCell(..), PossibleList2d, Rationale(..), Removal)
import Set
import SolversDirectLogic
import SolversReduction
import SolversTriplet


bruteForceRemoveInvalid : PossibleList2d -> PossibleList2d
bruteForceRemoveInvalid p2d =
    let
        actions =
            getPossibleActions p2d

        maybeInvalidAction =
            getInvalidAction p2d actions
    in
    case maybeInvalidAction of
        Nothing ->
            p2d

        Just invalidAction ->
            -- remove from possibles
            removeValueFromPossiblesForCell p2d invalidAction


getInvalidAction : PossibleList2d -> List Action -> Maybe Action
getInvalidAction p2d actions =
    case actions of
        [] ->
            Nothing

        action :: xs ->
            -- if valid, we cant infer anything
            if isValidafterApplyingAction p2d action then
                getInvalidAction p2d xs

            else
                Just action


isValidafterApplyingAction p2d action =
    let
        updatedP2d =
            Action.updatePossibleList2d action p2d
                |> SolversDirectLogic.removeFilledInSameRow
                |> SolversDirectLogic.removeFilledInSameColumn
                |> SolversDirectLogic.removeFilledInSameBox
                |> SolversReduction.valueOnlyPossibleInOneCellInRow
                |> SolversReduction.valueOnlyPossibleInOneCellInColumn
                |> SolversReduction.valueOnlyPossibleInOneCellInBox
                |> SolversTriplet.applyTripletLogic
    in
    PossibleList2d.isValid updatedP2d


getPossibleActions p2d =
    List.concat (List.map (\( x, y, cell ) -> getActionsFromIndexedCell x y cell) (List2d.indexedList p2d))


getActionsFromIndexedCell : Int -> Int -> PossibleCell -> List Action
getActionsFromIndexedCell row col cell =
    case cell of
        Filled _ ->
            []

        Possibles p ->
            List.map (\v -> Action row col v []) (Set.toList p.remaining)


removeValueFromPossiblesForCell : PossibleList2d -> Action -> PossibleList2d
removeValueFromPossiblesForCell p2d action =
    let
        cell =
            List2d.get action.x action.y p2d
    in
    case cell of
        -- nothing and filled shouldnt be possible
        Nothing ->
            p2d

        Just (Filled _) ->
            p2d

        Just (Possibles p) ->
            let
                newP =
                    Possibles { remaining = Set.diff p.remaining (Set.fromList [ action.value ]), removed = p.removed ++ [ Removal [ action.value ] ExhaustiveEnumerationInvalid ] }
            in
            List2d.set action.x action.y newP p2d
