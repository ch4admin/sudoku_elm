module Grid exposing
    ( Grid, Coordinate
    , set, get
    , fromList, getColumn, getRow, indexedList, indexedRows, initSquare, list, map, slice, toColumnsList, transposeList
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
import Maybe exposing (andThen)
import Tuple exposing (first, second)


{-| Data type representing a two-dimensional array. Use this to declare types
in your program. For example, if you wanted to have a `Grid` of Integers, you
could write this:
type alias MyType = Grid Int
-}
type alias Grid a =
    Array (Array a)


{-| Type to use when indexing into `Grid`.
-}
type alias Coordinate =
    { x : Int, y : Int }


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



--indexedList : Grid a -> List ( Int, Int, a )


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


list : Grid a -> List a
list grid =
    List.map (\( x, y, v ) -> v) (indexedList grid)


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



--
--transposeList : List (List a) -> List (List a)
--transposeList ll =
--  let
--
--      firstRow = List.head ll
--
--      getLength =
--          case List.head ll of
--              Nothing ->
--                  0
--              Just row ->
--                  List.length row
--
--      arr = Array.range
--
--  in
--
--
--
--
--
--  let heads = List.map (List.take 1) ll |> List.concat
--      tails = List.map (List.drop 1) ll
--  in
--      if | List.length heads == List.length ll ->
--             heads::(transpose tails)
--         | otherwise ->
--             []
--
--
--
--transpose : Grid a -> Grid a
--transpose a =
--  let
--    firstRow = getRow 0 a
--    restRows = dropRows 1 a
--  in
--    case firstRow of
--      Nothing -> a
--      Just firstRow' ->
--        let
--          vector = colVector firstRow'
--          step row acc = Array.Extra.map2 (Array.append) acc (colVector row)
--        in
--          Array.foldl step vector restRows
--transposeList : List (List a) -> Maybe (List (List a))
--transposeList ll =
--    case ll of
--        [] ->
--            Just []
--
--        [] :: xss ->
--            transposeList xss
--
--        (x :: xs) :: xss ->
--            let
--                heads =
--                    List.filterMap List.head xss
--
--                tails =
--                    List.filterMap List.tail xss
--            in
--            (x :: heads) :: transposeList (xs :: tails)
--get : Int -> Int -> Grid a -> Maybe a
--get row col grid =
--    getRow row array2d |> Maybe.andThen (Array.get col)
--oneOf : List (Maybe a) -> Maybe a
--oneOf maybes =
--    case maybes of
--        [] ->
--            Nothing
--
--        maybe :: rest ->
--            case maybe of
--                Nothing ->
--                    oneOf rest
--
--                Just _ ->
--                    maybe
--{-| Fetch the column at the given index.
--column 2 (square 3 (+)) == Array.fromList [2, 3, 4]
--column 2 (square 8 (^)) == Array.fromList [1, 2, 4, 8, 16, 32, 64, 128]
---}
--column : Int -> Grid a -> Maybe (Array a)
--column index grid =
--    let
--        got =
--            Array.map (Array.get index) grid
--    in
--    case oneOf (Array.toList got) of
--        Nothing ->
--            Nothing
--
--        Just _ ->
--            got
--                |> Array.map (Maybe.map (\x -> Array.fromList [ x ]))
--                |> Array.map (Maybe.withDefault Array.empty)
--                |> Array.foldr Array.append Array.empty
--                |> Just
--
--{-| Fetch the row at the given index.
--row 3 (repeat 1 4 "bun") == Just (Array.fromList ["bun"])
---}
--row : Int -> Grid a -> Maybe (Array a)
--row index grid =
--    Array.get index grid
--
--
--type alias Filler a =
--    Int -> Int -> a
--
--
--{-| Create a grid `width` units by `height` units, filling each cell according
--to the cell's coordinate.
--get (2, 1) (rectangle 4 2 (+)) == Just 3
---}
--rectangle : Int -> Int -> Filler a -> Grid a
--rectangle width height filler =
--    Array.initialize height (\y -> Array.initialize width (\x -> filler x y))
--
--
--{-| Like `rectangle`, except always make a square grid
---}
--square : Int -> Filler a -> Grid a
--square size filler =
--    rectangle size size filler
--
--
--{-| Create a grid just like [`Grid#rectangle`](Grid#rectangle), except just
--copy a value into each cell.
--get (2, 1) (rectangle 4 2 "foo") == Just "foo"
---}
--repeat : Int -> Int -> a -> Grid a
--repeat x y occupant =
--    rectangle x y (always << always occupant)
--
--
--{-| Like `repeat`, except make a square grid.
---}
--repeatSquare : Int -> a -> Grid a
--repeatSquare size occupant =
--    square size (always << always occupant)
--
--
--coordinate : Int -> Int -> Coordinate
--coordinate =
--    (,)
--
--
--{-| Fetch the column index from a `Coordinate`. Useful with `column`
---}
--toColumn : Coordinate -> Int
--toColumn =
--    first
--
--
--{-| Fetch the row index from a `Coordinate`. Useful with `row`.
---}
--toRow : Coordinate -> Int
--toRow =
--    second
--
--
--
---- TODO: Export or delete
---- width : Grid a -> Int
---- width grid =
----     Array.get 0 grid
----         |> Maybe.map Array.length
----         |> Maybe.withDefault 0
---- height : Grid a -> Int
---- height grid =
----     Array.length grid
--
--
--{-| Fetch the occupant at a given `Coordinate`.
--get (2,4) (square 6 (\*)) == Just 8
---}
--
--
--
----get : Coordinate -> Grid a -> Maybe a
----get coord grid =
----    Array.get (toRow coord) grid |> andThen Array.get (toColumn coord)
--
--
--get : Coordinate -> Grid a -> Maybe a
--get coord grid =
--    andThen (Array.get (toColumn coord)) (Array.get (toRow coord) grid)
--
--
--{-| Overwrite the occupant at a given `Coordinate`
---}
--set : Coordinate -> a -> Grid a -> Grid a
--set coord occupant grid =
--    row (toRow coord) grid
--        |> Maybe.map (Array.set (toColumn coord) occupant)
--        |> Maybe.map (\r -> Array.set (toRow coord) r grid)
--        |> Maybe.withDefault grid
--        |> Debug.log "new"
--
--
--map : (a -> b) -> Grid a -> Grid b
--map f grid =
--    Array.map (Array.map f) grid
--
--
--mapWithCoordinate : (Coordinate -> a -> b) -> Grid a -> Grid b
--mapWithCoordinate f grid =
--    Array.indexedMap
--        (\y -> Array.indexedMap (\x -> f (coordinate x y)))
--        grid
