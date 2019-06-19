module Solve exposing (solution, solutionFromPossibleGrid)

import PossibleTripletGrid
import Solvers
import SudokuGrid exposing (Action, PossibleGrid, SudokuGrid, initPossibleGrid, isSolved, updatePossibleGrid)


solution : SudokuGrid -> ( PossibleGrid, List Action )
solution sudokuGrid =
    solutionFromPossibleGrid (initPossibleGrid sudokuGrid) []


solutionFromPossibleGrid : PossibleGrid -> List Action -> ( PossibleGrid, List Action )
solutionFromPossibleGrid possibleGrid actions =
    if isSolved possibleGrid then
        ( possibleGrid, actions )

    else
        let
            -- modify possibleGrid
            newGrid =
                possibleGrid
                    |> Solvers.removeSameRow
                    |> Solvers.removeSameCol
                    |> Solvers.removeSameBox
                    |> Solvers.onlyPossibleValueInRow
                    |> Solvers.onlyPossibleValueInColumn
                    |> Solvers.onlyPossibleValueInBox
                    |> PossibleTripletGrid.applyTripletLogic

            action =
                Debug.log "Action:" (getAction Solvers.solversList newGrid)
        in
        case action of
            Nothing ->
                -- if grid is modified try again to generate an action
                -- otherwise just return
                if newGrid /= possibleGrid then
                    solutionFromPossibleGrid newGrid actions

                else
                    ( newGrid, actions )

            Just a ->
                -- apply action to update grid, and recurse
                let
                    pg =
                        updatePossibleGrid a newGrid
                in
                solutionFromPossibleGrid pg (actions ++ [ a ])


{-| direct logic. If run twice, will not result in a different answer
-}
applyDirectLogic : PossibleGrid -> PossibleGrid
applyDirectLogic pg =
    pg |> Solvers.removeSameRow |> Solvers.removeSameCol |> Solvers.removeSameBox



--iterate solvers to try to find an action


getAction : List (PossibleGrid -> Maybe Action) -> PossibleGrid -> Maybe Action
getAction solvers possibleGrid =
    case solvers of
        [] ->
            Nothing

        solver :: ss ->
            case solver possibleGrid of
                Nothing ->
                    getAction ss possibleGrid

                Just a ->
                    Just a
