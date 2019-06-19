module Grid exposing
    ( Grid
    , set, get
    , fromList, fromListOfBoxLists, getColumn, getRow, indexedList, indexedRows, initSquare, map, slice, toColumnsList, toFlattenedList, toList, toListOfBoxLists, transposeList
    )

{-| This library provides a data type to represent two-dimensional arrays.


# Definition

@docs Grid, Coordinate


# Creating `Grid`s

@docs rectangle, square, repeat, repeatSquare


# Get and set

@docs set, get, row, column


# Coordinates

@docs toColumn, toRow, toCoordinate

-}

import Array exposing (Array)
import List
import ListExtra
import Maybe exposing (andThen)
import Tuple exposing (first, second)


{-| Data type representing a two-dimensional array. Use this to declare types
in your program. For example, if you wanted to have a `Grid` of Integers, you
could write this:
type alias MyType = Grid Int
-}
type alias Grid a =
    Array (Array a)


fromList : List (List a) -> Grid a
fromList aa =
    Array.fromList (List.map Array.fromList aa)


initSquare length defaultValue =
    Array.initialize length (\y -> Array.initialize length (\x -> defaultValue))


getRow : Int -> Grid a -> Maybe (Array a)
getRow row grid =
    Array.get row grid


get : Int -> Int -> Grid a -> Maybe a
get row col grid =
    getRow row grid |> Maybe.andThen (Array.get col)


set : Int -> Int -> a -> Grid a -> Grid a
set row col newValue grid =
    getRow row grid
        |> Maybe.map (\rowArray -> Array.set row (Array.set col newValue rowArray) grid)
        |> Maybe.withDefault grid


map : (a -> b) -> Grid a -> Grid b
map f grid =
    Array.map (Array.map f) grid


{-| indexedList : Grid a -> List ( Int, Int, a )
-}
indexedRows : Grid a -> List ( Int, Array a )
indexedRows grid =
    Array.toIndexedList grid


getIndexedValuesFromIndexedRow : ( Int, Array a ) -> List ( Int, Int, a )
getIndexedValuesFromIndexedRow tuple =
    let
        ( x, array ) =
            tuple
    in
    List.map (\( i, v ) -> ( x, i, v )) (Array.toIndexedList array)


indexedList : Grid a -> List ( Int, Int, a )
indexedList grid =
    let
        lists =
            List.map getIndexedValuesFromIndexedRow (indexedRows grid)
    in
    List.concat lists


toList : Grid a -> List (List a)
toList grid =
    List.map Array.toList (Array.toList grid)


toFlattenedList : Grid a -> List a
toFlattenedList grid =
    List.concat (toList grid)



--    List.map (\( x, y, v ) -> v) (indexedList grid)


toColumnsList : Grid a -> List (Array a)
toColumnsList grid =
    let
        -- we need the column count which we get from the first row
        firstRow =
            Array.get 1 grid
    in
    case firstRow of
        Nothing ->
            []

        Just a ->
            let
                columns =
                    Array.length a
            in
            compactAndConvertToList (List.map (\i -> getColumn i grid) (List.range 0 (columns - 1)))


getColumn : Int -> Grid a -> Maybe (Array a)
getColumn c grid =
    let
        maybeValues =
            List.map (\arr -> Array.get c arr) (Array.toList grid)
    in
    if List.any (\x -> x == Nothing) maybeValues then
        Nothing

    else
        Just (Array.fromList (compactAndConvertToList maybeValues))


compactAndConvertToList : List (Maybe a) -> List a
compactAndConvertToList arr =
    List.filterMap identity arr


transposeList : List (List a) -> List (List a)
transposeList listOfLists =
    List.foldr (List.map2 (::)) (List.repeat (rowsLength listOfLists) []) listOfLists


rowsLength : List (List a) -> Int
rowsLength listOfLists =
    case listOfLists of
        [] ->
            0

        x :: _ ->
            List.length x


slice : Int -> Int -> Int -> Int -> Grid a -> Grid a
slice fromRow toRow fromCol toCol grid =
    let
        arr =
            Array.slice fromRow toRow grid
    in
    Array.fromList (List.map (\a -> Array.slice fromCol toCol a) (Array.toList arr))



-- take a grid, and extract boxes of width / height as individual lists of items


toListOfBoxLists : Int -> Int -> Grid a -> List (List a)
toListOfBoxLists rowsPerBox columnsPerBox g =
    makeToListOfBoxLists rowsPerBox columnsPerBox (toList g) []


makeToListOfBoxLists : Int -> Int -> List (List a) -> List (List a) -> List (List a)
makeToListOfBoxLists rowsPerBox columnsPerBox lst output =
    case lst of
        [] ->
            output

        nonEmptyList ->
            let
                ( beginning, remainder ) =
                    ListExtra.splitAt rowsPerBox nonEmptyList
            in
            case beginning of
                [] ->
                    -- move onto remainder if empty
                    makeToListOfBoxLists rowsPerBox columnsPerBox remainder output

                rows ->
                    let
                        box =
                            List.concat (List.map (\row -> List.take columnsPerBox row) rows)

                        rowRemainders =
                            let
                                remainders =
                                    List.map (\r -> List.drop columnsPerBox r) rows
                            in
                            if List.any (\r -> List.isEmpty r) remainders then
                                []

                            else
                                remainders
                    in
                    makeToListOfBoxLists rowsPerBox columnsPerBox (rowRemainders ++ remainder) (output ++ [ box ])


fromListOfBoxLists : Int -> Int -> List (List a) -> Grid a
fromListOfBoxLists rowsPerBox columnsPerBox lst =
    makeFromListOfBoxLists rowsPerBox columnsPerBox lst []


makeFromListOfBoxLists : Int -> Int -> List (List a) -> List (List a) -> Grid a
makeFromListOfBoxLists rowsPerBox columnsPerBox lst output =
    case lst of
        [] ->
            fromList output

        nonEmptyList ->
            let
                ( beginning, remainder ) =
                    ListExtra.splitAt rowsPerBox nonEmptyList
            in
            -- if no values, move on to the next set of 3 lists
            if List.any List.isEmpty beginning then
                makeFromListOfBoxLists rowsPerBox columnsPerBox remainder output

            else
                let
                    row =
                        List.concat (List.map (\l -> List.take columnsPerBox l) beginning)

                    adjLists =
                        List.map (\l -> List.drop columnsPerBox l) beginning
                in
                makeFromListOfBoxLists rowsPerBox columnsPerBox (adjLists ++ remainder) (output ++ [ row ])
