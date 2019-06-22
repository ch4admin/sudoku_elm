module Solve exposing (getSolution, solutionFromPossibleList2d)

import Action exposing (Action)
import PossibleList2d exposing (PossibleList2d)
import SolversDirectLogic
import SolversReduction
import SudokuGrid exposing (SudokuGrid)


getSolution : SudokuGrid -> ( PossibleList2d, List Action )
getSolution sudokuGrid =
    PossibleList2d.fromSudokuGrid sudokuGrid |> solutionFromPossibleList2d []


solutionFromPossibleList2d : List Action -> PossibleList2d -> ( PossibleList2d, List Action )
solutionFromPossibleList2d actions p2d =
    if PossibleList2d.isSolved p2d then
        ( p2d, actions )

    else
        let
            -- modify possibleGrid
            newP2d =
                p2d
                    |> SolversDirectLogic.removeFilledInSameRow
                    |> SolversDirectLogic.removeFilledInSameColumn
                    |> SolversDirectLogic.removeFilledInSameBox
                    |> SolversReduction.valueOnlyPossibleInOneCellInRow
                    |> SolversReduction.valueOnlyPossibleInOneCellInColumn
                    |> SolversReduction.valueOnlyPossibleInOneCellInBox

            --                    |> PossibleTripletList2d.applyTripletLogic
            maybeAction =
                Action.getAction newP2d
        in
        case maybeAction of
            Nothing ->
                -- if grid is modified try again to generate an action
                -- otherwise just return
                if newP2d /= p2d then
                    solutionFromPossibleList2d actions newP2d

                else
                    ( newP2d, actions )

            Just a ->
                -- apply action to update grid, and recurse
                solutionFromPossibleList2d (actions ++ [ a ]) (Action.updatePossibleList2d a newP2d)
