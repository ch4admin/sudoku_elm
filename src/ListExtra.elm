module ListExtra exposing (frequencies, groupsOf, splitAt)

import Dict


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


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


frequencies : List comparable -> Dict.Dict comparable Int
frequencies list =
    list
        |> List.foldl
            (\el counter ->
                Dict.get el counter
                    |> Maybe.withDefault 0
                    |> (\count -> count + 1)
                    |> (\count -> Dict.insert el count counter)
            )
            Dict.empty
