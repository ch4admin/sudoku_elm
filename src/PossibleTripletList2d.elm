module PossibleTripletList2d exposing
    ( PossibleTripletCell
    , PossibleTripletList2d
    , columnTripletsFromPossibleList2d
    , rowTripletsfromPossibleList2d
    , tripletFromPossibleCells
    )

import Array exposing (Array)
import Grid
import List2d exposing (List2d)
import ListExtra
import PossibleList2d exposing (PossibleCell(..), PossibleList2d, Rationale(..), Removal)
import Set


type alias PossibleTripletList2d =
    List2d PossibleTripletCell


type alias PossibleTripletCell =
    { values : Set.Set Int, remaining : Set.Set Int }


rowTripletsfromPossibleList2d : PossibleList2d -> PossibleTripletList2d
rowTripletsfromPossibleList2d p2d =
    let
        tripletRowsFromRow : List PossibleCell -> List PossibleTripletCell
        tripletRowsFromRow row =
            List.map tripletFromPossibleCells (ListExtra.groupsOf 3 row)
    in
    List.map tripletRowsFromRow p2d


columnTripletsFromPossibleList2d : PossibleList2d -> PossibleTripletList2d
columnTripletsFromPossibleList2d p2d =
    p2d |> List2d.transpose |> rowTripletsfromPossibleList2d |> List2d.transpose



--    let
--        groups =
--            List2d.toListOfBoxLists 3 1 p2d
--
--        triplets =
--            List.map tripletFromPossibleCells groups
--    in
--    ListExtra.groupsOf 9 triplets


tripletFromPossibleCells : List PossibleCell -> PossibleTripletCell
tripletFromPossibleCells row =
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
