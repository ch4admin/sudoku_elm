module SolversTriplet exposing
    ( applyTripletLogic
    , updatePossibleCellFromTriplet
    , updatePossibleCellsFromTripletCell
    , updatePossibleGridFromRowTriplets
    , updateRow
    )

import List2d
import ListExtra
import PossibleList2d exposing (PossibleCell(..), PossibleList2d, Rationale(..), Removal)
import PossibleTripletList2d exposing (PossibleTripletCell, PossibleTripletList2d)
import Set


applyTripletLogic : PossibleList2d -> PossibleList2d
applyTripletLogic p2d =
    let
        ptgFromRows =
            p2d
                |> PossibleTripletList2d.rowTripletsfromPossibleList2d
                |> applyRowTripletLogic

        newP2d =
            updatePossibleGridFromRowTriplets ptgFromRows p2d

        ptgFromCols =
            newP2d |> PossibleTripletList2d.columnTripletsFromPossibleList2d |> applyColumnTripletLogic
    in
    updatePossibleGridFromColumnTriplets ptgFromCols newP2d


{-| apply any logic from triplet grid to possiblegrid
-}
updatePossibleGridFromRowTriplets : PossibleTripletList2d -> PossibleList2d -> PossibleList2d
updatePossibleGridFromRowTriplets ptL2d pl2d =
    List.map2 (\x y -> updateRow x y) ptL2d pl2d


updatePossibleGridFromColumnTriplets : PossibleTripletList2d -> PossibleList2d -> PossibleList2d
updatePossibleGridFromColumnTriplets ptL2d pl2d =
    updatePossibleGridFromRowTriplets (List2d.transpose ptL2d) (List2d.transpose pl2d) |> List2d.transpose


updateRow : List PossibleTripletCell -> List PossibleCell -> List PossibleCell
updateRow ptL2dRow p2dRow =
    let
        groupedCells =
            ListExtra.groupsOf 3 p2dRow
    in
    List.map2 updatePossibleCellsFromTripletCell ptL2dRow groupedCells |> List.concat


{-| For each possibleCell, remove values are not present in the filled or remaining values for the triplet
-}
updatePossibleCellsFromTripletCell : PossibleTripletCell -> List PossibleCell -> List PossibleCell
updatePossibleCellsFromTripletCell possibleTripletCell possibleCells =
    let
        possibleValues =
            Set.union possibleTripletCell.values possibleTripletCell.remaining
    in
    List.map (\c -> updatePossibleCellFromTriplet possibleValues c) possibleCells


updatePossibleCellFromTriplet : Set.Set Int -> PossibleCell -> PossibleCell
updatePossibleCellFromTriplet tripletRemaining c =
    case c of
        Filled v ->
            Filled v

        Possibles p ->
            let
                -- if cant exist in triplet, then cant exist in PossibleCell
                newRemaining =
                    Set.intersect p.remaining tripletRemaining
            in
            if newRemaining == p.remaining then
                Possibles p

            else
                let
                    removedValues =
                        Set.toList (Set.diff p.remaining newRemaining)
                in
                Possibles { remaining = newRemaining, removed = p.removed ++ [ Removal removedValues BoxRowLogic ] }


{-| Input is a List2d of 9 row and 3 columns
From this we want to extract boxes, which are 3 x 1 groups
-}
applyRowTripletLogic : PossibleTripletList2d -> PossibleTripletList2d
applyRowTripletLogic ptL2d =
    let
        boxes =
            List2d.toListOfBoxLists 3 1 ptL2d

        -- apply logic to each box
        -- if a triplet only has 3 possible values, then they are all fills
        -- remove filled values from other triplet possibilities in the box
        -- fill where value is only possible in one triplet
        -- remove filled values from other triplet possibilities in the box
        adjustedBoxes =
            boxes
                |> List2d.map fillWhereOnlyThreePossibilities
                |> List.map removePossibleWhereFilled
                |> List.map fillWhereValueOnlyPossibleInOneCell

        -- reassemble into rows
        rows =
            List2d.fromListOfBoxLists 3 1 adjustedBoxes
    in
    -- now apply logic to each row, based on adjustments from above
    List.map removePossibleWhereFilled rows


applyColumnTripletLogic : PossibleTripletList2d -> PossibleTripletList2d
applyColumnTripletLogic ptL2d =
    ptL2d |> List2d.transpose |> applyRowTripletLogic |> List2d.transpose



--    ptL2d
--        |> List2d.transpose
--        |> List2d.toListOfBoxLists 3 1
--        |> List2d.map fillWhereOnlyThreePossibilities
--        |> List.map removePossibleWhereFilled
--        |> List2d.fromListOfBoxLists 3 1
--        |> List2d.transpose
--    let
--        boxes =
--            List2d.toListOfBoxLists 1 3 ptL2d
--
--        adjustedBoxes =
--            List.map fillWhereValueOnlyPossibleInOneCell boxes
--
--        -- reassemble into rows, 3 x 9
--        rows =
--            List2d.fromListOfBoxLists 1 3 adjustedBoxes
--
--        newRows =
--            List.map removePossibleWhereFilled rows
--    in
--    rows


fillWhereValueOnlyPossibleInOneCell : List PossibleTripletCell -> List PossibleTripletCell
fillWhereValueOnlyPossibleInOneCell ptL2ds =
    let
        possibles =
            List.concat (List.map (\c -> Set.toList c.remaining) ptL2ds)

        isUnique v =
            List.length (List.filter (\x -> x == v) possibles) == 1

        uniques =
            Set.fromList (List.filter isUnique possibles)
    in
    List.map (\ptc -> fillPossibleTripletCell uniques ptc) ptL2ds


{-| move matching uniques to filled for the cell
-}
fillPossibleTripletCell : Set.Set Int -> PossibleTripletCell -> PossibleTripletCell
fillPossibleTripletCell uniques ptc =
    let
        matchingUniques =
            Set.intersect uniques ptc.remaining

        filled =
            Set.union ptc.values matchingUniques

        -- no more possibles if we have a filled set
        remaining =
            if Set.size filled == 3 then
                Set.empty

            else
                Set.diff ptc.remaining uniques
    in
    PossibleTripletCell filled remaining


{-| Apply to a row or column
-}
removePossibleWhereFilled : List PossibleTripletCell -> List PossibleTripletCell
removePossibleWhereFilled ptcs =
    let
        -- get all filled values in the row
        filled =
            ptcs |> List.map (\c -> Set.toList c.values) |> List.concat |> Set.fromList

        removeFilled c =
            PossibleTripletCell c.values (Set.diff c.remaining filled)

        --        filled = Set.fromList (List.concat (List.map (\c -> Set.toList c.values) row))
    in
    List.map removeFilled ptcs


{-| PossibleTripletCell has 3 values. If we have 1 filled, 2 possibles, we can fill the remainder
-}
fillWhereOnlyThreePossibilities ptc =
    let
        allValues =
            Set.union ptc.values ptc.remaining
    in
    if Set.size allValues == 3 then
        PossibleTripletCell allValues Set.empty

    else
        ptc
