module List2d exposing (List2d, fromListOfBoxLists, get, indexedList, map, set, toListOfBoxLists, transpose)

import ListExtra


type alias List2d a =
    List (List a)


map : (a -> b) -> List2d a -> List2d b
map f l2d =
    List.map (List.map f) l2d


get : Int -> Int -> List2d a -> Maybe a
get row col l2d =
    let
        maybeMatchedRow =
            List.drop row l2d |> List.head
    in
    case maybeMatchedRow of
        Nothing ->
            Nothing

        Just matchedRow ->
            List.drop col matchedRow |> List.head


set : Int -> Int -> a -> List2d a -> List2d a
set row col newValue l2d =
    let
        processRow : Int -> List a -> List a
        processRow i r =
            if i == row then
                List.indexedMap
                    (\j v ->
                        if j == col then
                            newValue

                        else
                            v
                    )
                    r

            else
                r
    in
    List.indexedMap processRow l2d


transpose : List2d a -> List2d a
transpose l2d =
    List.foldr (List.map2 (::)) (List.repeat (rowsLength l2d) []) l2d


rowsLength : List2d a -> Int
rowsLength listOfLists =
    case listOfLists of
        [] ->
            0

        x :: _ ->
            List.length x


indexedList : List2d a -> List ( Int, Int, a )
indexedList l2d =
    List.indexedMap (\i row -> List.indexedMap (\j v -> ( i, j, v )) row) l2d
        |> List.concat


toListOfBoxLists : Int -> Int -> List2d a -> List2d a
toListOfBoxLists rowsPerBox columnsPerBox l2d =
    makeToListOfBoxLists rowsPerBox columnsPerBox l2d []


makeToListOfBoxLists : Int -> Int -> List2d a -> List2d a -> List2d a
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


fromListOfBoxLists : Int -> Int -> List2d a -> List2d a
fromListOfBoxLists rowsPerBox columnsPerBox lst =
    makeFromListOfBoxLists rowsPerBox columnsPerBox lst []


makeFromListOfBoxLists : Int -> Int -> List2d a -> List2d a -> List2d a
makeFromListOfBoxLists rowsPerBox columnsPerBox lst output =
    case lst of
        [] ->
            output

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
