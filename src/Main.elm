module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug
import Grid
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Puzzles exposing (Puzzle, puzzles)
import Set
import Solve
import SudokuGrid exposing (Action, PossibleCell(..), PossibleGrid, SudokuGrid)
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
    , solution : Maybe PossibleGrid
    , actions : List Action
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url (List.head puzzles) puzzles False Nothing [], Cmd.none )



-- UPDATE


type Msg
    = SelectPuzzle String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | SolvePuzzle


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
                            Solve.solution p.puzzle
                    in
                    ( { model
                        | solution = Just solution
                        , actions = actions
                        , solved = True
                      }
                    , Cmd.none
                    )



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
                [ div [ class "ten wide column" ] [ sudokuBoardView (gridFromPuzzle model.puzzle) model.solution model.solved ]
                , div [ class "six wide column" ]
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


sudokuBoardView : SudokuGrid -> Maybe PossibleGrid -> Bool -> Html Msg
sudokuBoardView originalGrid solutionGrid solved =
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
            div [ class "skuBoxRow" ] (List.map (\m -> cell row (col m) solved originalGrid solutionGrid) (List.range 0 2))

        boardRows =
            List.map boardRow (List.range 0 2)
    in
    div [ class "skuBoard" ] boardRows


cell : Int -> Int -> Bool -> SudokuGrid -> Maybe PossibleGrid -> Html Msg
cell row col solved originalGrid maybeSolutionGrid =
    let
        ov =
            originalValue row col originalGrid
    in
    case solved of
        True ->
            case ov of
                Just v ->
                    div [ class "skuCell" ] [ text (String.fromInt v) ]

                Nothing ->
                    case maybeSolutionGrid of
                        Nothing ->
                            -- cant have no solution if solved
                            div [ class "skuCell" ] [ text "" ]

                        Just solutionGrid ->
                            case Grid.get row col solutionGrid of
                                Nothing ->
                                    div [ class "skuCell" ] [ text "" ]

                                Just (Filled v) ->
                                    div [ class "skuCell solvedColor" ] [ text (String.fromInt v) ]

                                Just (Possibles p) ->
                                    let
                                        str =
                                            String.join " " (List.map String.fromInt (Set.toList p.remaining))
                                    in
                                    div [ class "skuCell possiblesSize" ] [ text str ]

        False ->
            -- show sudoku grid before being solved
            case ov of
                Nothing ->
                    div [ class "skuCell" ] [ text "" ]

                Just v ->
                    div [ class "skuCell" ] [ text (String.fromInt v) ]


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


cellValue : Int -> Int -> SudokuGrid -> String
cellValue row col grid =
    case Grid.get row col grid of
        Nothing ->
            ""

        Just v ->
            case v of
                Nothing ->
                    ""

                Just y ->
                    String.fromInt y


actionView actions =
    let
        row action =
            tr []
                [ th [] [ text (String.fromInt action.x ++ ", " ++ String.fromInt action.y) ]
                , th [] [ text (String.fromInt action.value) ]
                , tr [] [ text "Rationale" ]
                ]

        rows =
            List.map row actions
    in
    table [ class "ui table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Cell" ]
                , th [] [ text "Detail" ]
                ]
            ]
        , tbody [] rows
        ]
