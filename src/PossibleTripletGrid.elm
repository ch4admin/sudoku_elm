module PossibleTripletGrid exposing
    ( PossibleTripletCell
    , PossibleTripletGrid
    , fromPossibleGrid
    , tripletFromRow
    )

import Array exposing (Array)
import Grid
import ListExtra
import Set
import SudokuGrid exposing (PossibleCell(..), PossibleGrid, Rationale(..), Removal)


type alias PossibleTripletGrid =
    Grid.Grid PossibleTripletCell


type alias PossibleTripletCell =
    { values : Set.Set Int, remaining : Set.Set Int }


fromPossibleGrid : PossibleGrid -> PossibleTripletGrid
fromPossibleGrid pg =
    Array.map tripletRowsFromRow pg


tripletRowsFromRow : Array PossibleCell -> Array PossibleTripletCell
tripletRowsFromRow row =
    Array.fromList (List.map tripletFromRow (groupsOf 3 (Array.toList row)))


tripletFromRow : List PossibleCell -> PossibleTripletCell
tripletFromRow row =
    let
        getValues pc =
            case pc of
                Possibles _ ->
                    []

                Filled v ->
                    [ v ]

        values =
            Set.fromList (List.concat (List.map getValues row))

        getPossibleValues pc =
            case pc of
                Possibles p ->
                    Set.toList p.remaining

                Filled _ ->
                    []

        remaining =
            Set.fromList (List.concat (List.map getPossibleValues row))
    in
    PossibleTripletCell values remaining



-- unpack a PTG into a list, each one a list of 3 TripletCells


toListOfBoxLists : PossibleTripletGrid -> List (List PossibleTripletCell)
toListOfBoxLists ptg =
    makeToListOfBoxLists (Grid.toList ptg) []


makeToListOfBoxLists lst output =
    case lst of
        [] ->
            output

        nonEmptyLst ->
            let
                ( beginning, remainder ) =
                    ListExtra.splitAt 3 nonEmptyLst
            in
            case beginning of
                [] ->
                    makeToListOfBoxLists remainder output

                rows ->
                    let
                        box =
                            List.concat (List.map (\row -> List.take 3 row) rows)

                        rowRemainders =
                            List.map (\row -> List.drop 3 row) rows
                    in
                    makeToListOfBoxLists (rowRemainders ++ remainder) (output ++ [ box ])



--from List.Extra


groupsOf : Int -> List a -> List (List a)
groupsOf size xs =
    groupsOfWithStep size size xs


groupsOfWithStep : Int -> Int -> List a -> List (List a)
groupsOfWithStep size step xs =
    let
        group =
            List.take size xs

        xs_ =
            List.drop step xs

        okayArgs =
            size > 0 && step > 0

        okayLength =
            size == List.length group
    in
    if okayArgs && okayLength then
        group :: groupsOfWithStep size step xs_

    else
        []



-- create a PossibleTriplet Grid.  Apply logic.  Then apply back to an updated possiblegrid


applyTripletLogic : PossibleGrid -> PossibleGrid
applyTripletLogic pg =
    let
        ptg =
            pg |> fromPossibleGrid |> applyBoxRowLogic
    in
    updatePossibleGrid ptg pg



--apply any logic from triplet grid to possiblegrid


updatePossibleGrid : PossibleTripletGrid -> PossibleGrid -> PossibleGrid
updatePossibleGrid ptg pg =
    List.map2 (\x y -> updateRow (Array.toList x) (Array.toList y) []) (Array.toList ptg) (Array.toList pg) |> Grid.fromList


updateRow : List PossibleTripletCell -> List PossibleCell -> List PossibleCell -> List PossibleCell
updateRow ptgRow pgRow output =
    case ptgRow of
        [] ->
            output

        ptg :: ptgRemainder ->
            let
                pgs =
                    List.take 3 pgRow

                newOutput =
                    output ++ updatePossibleCellsFromBoxRow ptg pgs
            in
            updateRow ptgRemainder (List.drop 3 pgRow) newOutput



--


updatePossibleCellsFromBoxRow possibleTripletCell possibleCells =
    List.map (\c -> updatePossibleCellFromTriplet possibleTripletCell.remaining c) possibleCells


updatePossibleCellFromTriplet : Set.Set Int -> PossibleCell -> PossibleCell
updatePossibleCellFromTriplet tripletRemaining c =
    case c of
        Possibles p ->
            let
                -- if cant exist in triplet, then cant exist in PossibleCell
                newRemaining =
                    Set.diff p.remaining tripletRemaining
            in
            if newRemaining == p.remaining then
                Possibles p

            else
                let
                    removedValues =
                        Set.toList (Set.diff p.remaining newRemaining)
                in
                Possibles { remaining = newRemaining, removed = p.removed ++ [ Removal removedValues BoxRowLogic ] }

        Filled v ->
            Filled v


applyBoxRowLogic : PossibleTripletGrid -> PossibleTripletGrid
applyBoxRowLogic ptg =
    let
        adjustedBoxes =
            List.map fillWhereOnlyOneCellPossible (Grid.toListOfBoxLists 3 1 ptg)

        -- reassemble into rows
        rows =
            []

        newRows =
            List.map removePossibleWhereFilled rows
    in
    Grid.fromList newRows


fillWhereOnlyOneCellPossible : List PossibleTripletCell -> List PossibleTripletCell
fillWhereOnlyOneCellPossible boxOfptg =
    let
        possibles =
            List.concat (List.map (\c -> Set.toList c.values) boxOfptg)

        isUnique v =
            List.length (List.filter (\x -> x == v) possibles) == 1

        uniques =
            Set.fromList (List.filter isUnique possibles)
    in
    List.map (\ptc -> fillPossibleTripletCell uniques ptc) boxOfptg



-- move matching uniques to filled for the cell


fillPossibleTripletCell : Set.Set Int -> PossibleTripletCell -> PossibleTripletCell
fillPossibleTripletCell uniques ptc =
    let
        matchingUniques =
            Set.intersect uniques ptc.remaining

        filled =
            Set.union ptc.values matchingUniques

        remaining =
            Set.diff ptc.remaining uniques
    in
    PossibleTripletCell filled remaining



-- generate list of cells for each box


removePossibleWhereFilled row =
    let
        filled =
            row |> List.map (\c -> Set.toList c.values) |> List.concat |> Set.fromList

        removeFilled c =
            PossibleTripletCell c.filled (Set.diff c.remaining filled)

        --        filled = Set.fromList (List.concat (List.map (\c -> Set.toList c.values) row))
    in
    List.map removeFilled row
