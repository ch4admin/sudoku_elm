module SolversTriplet exposing (applyBoxColumnLogic, applyBoxRowLogic, applyTripletLogic, fillPossibleTripletCell, fillWhereValueOnlyPossibleInOneCell, removePossibleWhereFilled, updatePossibleCellFromTriplet, updatePossibleCellsFromBoxRow, updatePossibleGridFromRowTriplets, updateRow)


applyTripletLogic : PossibleGrid -> PossibleGrid
applyTripletLogic pg =
    let
        ptgFromRows =
            pg |> rowTripletsfromPossibleList2d |> applyBoxRowLogic

        newPg =
            updatePossibleGridFromRowTriplets ptgFromRows pg

        ptgFromCols =
            newPg |> columnTripletsFromPossibleList2d
    in
    updatePossibleGridFromRowTriplets ptgFromRows pg


{-| apply any logic from triplet grid to possiblegrid
-}
updatePossibleGridFromRowTriplets : PossibleTripletList2d -> PossibleGrid -> PossibleGrid
updatePossibleGridFromRowTriplets ptg pg =
    List.map2 (\x y -> updateRow (Array.toList x) (Array.toList y) []) (Array.toList ptg) (Array.toList pg) |> Grid.fromList2d


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
    let
        -- values that can remain in each cell are both filled and remaining values for the tripled
        possibleValues =
            Set.union possibleTripletCell.values possibleTripletCell.remaining
    in
    List.map (\c -> updatePossibleCellFromTriplet possibleValues c) possibleCells


updatePossibleCellFromTriplet : Set.Set Int -> PossibleCell -> PossibleCell
updatePossibleCellFromTriplet tripletRemaining c =
    case c of
        Possibles p ->
            let
                -- if cant exist in triplet, then cant exist in PossibleCell
                invalidValues =
                    Set.diff p.remaining tripletRemaining

                newRemaining =
                    Set.diff p.remaining invalidValues
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


applyBoxRowLogic : PossibleTripletList2d -> PossibleTripletList2d
applyBoxRowLogic ptg =
    let
        adjustedBoxes =
            List.map fillWhereValueOnlyPossibleInOneCell (Grid.toListOfBoxLists 3 1 ptg)

        -- reassemble into rows
        rows =
            Grid.toListOfList (Grid.fromListOfBoxLists 3 1 adjustedBoxes)

        newRows =
            List.map removePossibleWhereFilled rows
    in
    Grid.fromList2d newRows


applyBoxColumnLogic : PossibleTripletList2d -> PossibleTripletList2d
applyBoxColumnLogic ptg =
    let
        adjustedBoxes =
            List.map fillWhereValueOnlyPossibleInOneCell (Grid.toListOfBoxLists 1 3 ptg)

        -- reassemble into rows
        rows =
            Grid.toListOfList (Grid.fromListOfBoxLists 1 3 adjustedBoxes)

        newRows =
            List.map removePossibleWhereFilled rows
    in
    Grid.fromList2d newRows


fillWhereValueOnlyPossibleInOneCell : List PossibleTripletCell -> List PossibleTripletCell
fillWhereValueOnlyPossibleInOneCell boxOfptg =
    let
        possibles =
            List.concat (List.map (\c -> Set.toList c.remaining) boxOfptg)

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

        -- no more possibles if we have a filled set
        remaining =
            if Set.size filled == 3 then
                Set.empty

            else
                Set.diff ptc.remaining uniques

        --        remaining =
        --            Set.diff ptc.remaining uniques
    in
    PossibleTripletCell filled remaining



-- generate list of cells for each box


removePossibleWhereFilled : List PossibleTripletCell -> List PossibleTripletCell
removePossibleWhereFilled row =
    let
        -- get all filled values in the row
        filled =
            row |> List.map (\c -> Set.toList c.values) |> List.concat |> Set.fromList

        removeFilled c =
            PossibleTripletCell c.values (Set.diff c.remaining filled)

        --        filled = Set.fromList (List.concat (List.map (\c -> Set.toList c.values) row))
    in
    List.map removeFilled row
