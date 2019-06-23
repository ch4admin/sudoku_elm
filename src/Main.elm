module Main exposing (main)

import Action exposing (Action)
import Browser
import Browser.Navigation as Nav
import Debug
import Grid
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseOver)
import List2d
import PossibleList2d exposing (PossibleCell(..), PossibleList2d, Rationale(..), Removal)
import Puzzles exposing (Puzzle, puzzles)
import Set
import Solve
import SudokuGrid exposing (SudokuGrid)
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , puzzle : Maybe Puzzle
    , puzzles : List Puzzle
    , solved : Bool
    , solution : Maybe PossibleList2d
    , actions : List Action
    , hoverCell : Maybe ( Int, Int )
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url (Puzzles.puzzleFromName "5897") puzzles False Nothing [] Nothing, Cmd.none )



-- UPDATE


type Msg
    = SelectPuzzle String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | SolvePuzzle
    | HoverCell Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPuzzle name ->
            ( { model | puzzle = Puzzles.puzzleFromName name }, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        SolvePuzzle ->
            case model.puzzle of
                Nothing ->
                    ( model, Cmd.none )

                Just p ->
                    let
                        ( solution, actions ) =
                            Solve.getSolution p.puzzle
                    in
                    ( { model
                        | solution = Just solution
                        , actions = actions
                        , solved = True
                      }
                    , Cmd.none
                    )

        HoverCell x y ->
            ( { model | hoverCell = Just ( x, y ) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        solveButtonClass =
            if model.solved then
                "ui primary disabled button"

            else
                "ui primary button"
    in
    { title = "Sudoku Solver Elm"
    , body =
        [ div [ class "ui container" ]
            [ div [ class "ui grid" ]
                [ div [ class "seven wide column" ] [ sudokuBoardView (gridFromPuzzle model.puzzle) model.solution model.solved model.hoverCell ]
                , div [ class "nine wide column" ]
                    [ button [ class solveButtonClass, onClick SolvePuzzle ] [ text "Solve" ]
                    , actionView model.actions
                    ]
                ]
            ]
        ]
    }


gridFromPuzzle : Maybe Puzzle -> SudokuGrid.SudokuGrid
gridFromPuzzle puzzle =
    case puzzle of
        Nothing ->
            SudokuGrid.initEmpty

        Just p ->
            p.puzzle


sudokuBoardView : SudokuGrid -> Maybe PossibleList2d -> Bool -> Maybe ( Int, Int ) -> Html Msg
sudokuBoardView originalGrid solutionP2d isSolved hoverCell =
    let
        boardRow i =
            div [ class "skuBoardRow" ] (List.map (\j -> box i j) (List.range 0 2))

        box i j =
            let
                row k =
                    i * 3 + k
            in
            div [ class "skuBox" ] (List.map (\k -> boxRow (row k) j) (List.range 0 2))

        boxRow row j =
            let
                col m =
                    j * 3 + m
            in
            div [ class "skuBoxRow" ] (List.map (\m -> cell row (col m) isSolved originalGrid solutionP2d hoverCell) (List.range 0 2))

        boardRows =
            List.map boardRow (List.range 0 2)
    in
    div [ class "skuBoard" ] boardRows


hover : Int -> Int -> Maybe ( Int, Int ) -> Bool
hover x y hoverCell =
    case hoverCell of
        Nothing ->
            False

        Just ( hx, hy ) ->
            hx == x && hy == y


cell : Int -> Int -> Bool -> SudokuGrid -> Maybe PossibleList2d -> Maybe ( Int, Int ) -> Html Msg
cell row col solved originalGrid maybeSolutionP2d hoverCell =
    let
        ov =
            originalValue row col originalGrid

        hoverClass =
            if hover row col hoverCell then
                " hoverColor"

            else
                ""
    in
    case solved of
        True ->
            case ov of
                Just v ->
                    div [ class ("skuCell" ++ hoverClass) ] [ text (String.fromInt v) ]

                Nothing ->
                    case maybeSolutionP2d of
                        Nothing ->
                            -- cant have no solution if solved
                            div [ class ("skuCell" ++ hoverClass) ] [ text "" ]

                        Just solutionP2d ->
                            case List2d.get row col solutionP2d of
                                Nothing ->
                                    div [ class ("skuCell" ++ hoverClass) ] [ text "" ]

                                Just (Filled v) ->
                                    div [ class ("skuCell solvedColor" ++ hoverClass) ] [ text (String.fromInt v) ]

                                Just (Possibles p) ->
                                    let
                                        str =
                                            String.join " " (List.map String.fromInt (Set.toList p.remaining))
                                    in
                                    div [ class ("skuCell possiblesSize" ++ hoverClass) ] [ text str ]

        False ->
            -- show sudoku grid before being solved
            case ov of
                Nothing ->
                    div [ class ("skuCell" ++ hoverClass) ] [ text "" ]

                Just v ->
                    div [ class ("skuCell" ++ hoverClass) ] [ text (String.fromInt v) ]


originalValue row col grid =
    case Grid.get row col grid of
        Nothing ->
            Nothing

        Just v ->
            case v of
                Nothing ->
                    Nothing

                Just y ->
                    Just y


actionView actions =
    let
        row action =
            tr [ onMouseOver (HoverCell action.x action.y) ]
                [ td []
                    [ h2 [ class "ui header" ] [ text ("R" ++ String.fromInt (action.x + 1) ++ " C" ++ String.fromInt (action.y + 1)) ]
                    ]
                , td []
                    [ h2 [ class "ui center aligned header solvedColor" ] [ text (String.fromInt action.value) ]
                    ]
                , td [] [ getLogicText action.removed action.value ]
                ]

        getLogicText removed value =
            div [ class "ui list" ] (List.map (\r -> div [] [ text (rationaleText r value) ]) removed)

        rows =
            List.map row actions
    in
    table [ class "ui table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Cell" ]
                , th [] [ text "Value" ]
                , th [] [ text "Logic" ]
                ]
            ]
        , tbody [] rows
        ]


rationaleText : Removal -> Int -> String
rationaleText removal value =
    let
        logicText =
            case removal.rationale of
                SameRow ->
                    "Values exist in same row"

                SameColumn ->
                    "Values exist in same column"

                SameBox ->
                    "Values exist in same box"

                ValueOnlyPossibleInOneCellInRow ->
                    String.fromInt value ++ " is not possible in any other cell in this row"

                ValueOnlyPossibleInOneCellInColumn ->
                    String.fromInt value ++ " is not possible in any other cell in this column"

                ValueOnlyPossibleInOneCellInBox ->
                    String.fromInt value ++ " is not possible in any other cell in this box"

                BoxRowLogic ->
                    "BoxRowLogic"

                ExhaustiveEnumerationInvalid ->
                    "Exhaustive enumeration invalidated these values"
    in
    "Not [" ++ String.join "," (List.map String.fromInt removal.values) ++ "]: " ++ logicText
