module Grid exposing
    ( Grid
    , set, get
    , fromList2d, getColumn, getRow, indexedList, indexedRows, initSquare, map, slice, toColumnsList, toFlattenedList, toListOfList
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


fromList2d : List (List a) -> Grid a
fromList2d aa =
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


toListOfList : Grid a -> List (List a)
toListOfList grid =
    List.map Array.toList (Array.toList grid)


toFlattenedList : Grid a -> List a
toFlattenedList grid =
    List.concat (toListOfList grid)



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


slice : Int -> Int -> Int -> Int -> Grid a -> Grid a
slice fromRow toRow fromCol toCol grid =
    let
        arr =
            Array.slice fromRow toRow grid
    in
    Array.fromList (List.map (\a -> Array.slice fromCol toCol a) (Array.toList arr))



-- take a grid, and extract boxes of width / height as individual lists of items
