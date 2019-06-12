module Solve exposing (solution, solutionFromPossibleGrid)

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
                Solvers.removeSameRow possibleGrid |> Solvers.removeSameCol |> Solvers.removeSameBox

            action =
                Debug.log "Action:" (getAction Solvers.solversList newGrid)
        in
        case action of
            Nothing ->
                -- if cant solve just return
                ( newGrid, actions )

            Just a ->
                -- apply action to update grid, and recurse
                let
                    pg =
                        updatePossibleGrid a newGrid
                in
                solutionFromPossibleGrid pg (actions ++ [ Debug.log "a" a ])



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
