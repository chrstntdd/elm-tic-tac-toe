module Main exposing (..)

import Array exposing (Array, fromList, get, map, slice, toList)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Html exposing (Html, footer)
import MyStyles exposing (..)
import Tuple exposing (first, second)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }



-- Util functions


getMark : BoardState -> Int -> Maybe Mark
getMark boardState index =
    Array.get index boardState
        |> Maybe.withDefault Nothing


isX : Maybe Mark -> Bool
isX mark =
    case mark of
        Just X ->
            True

        _ ->
            False


isO : Maybe Mark -> Bool
isO mark =
    case mark of
        Just O ->
            True

        _ ->
            False


maybe : Maybe a -> a
maybe maybeA =
    case maybeA of
        Just a ->
            a

        Nothing ->
            Debug.crash "Can't unwrap that maybe"


type Mark
    = X
    | O


type alias Play =
    ( Maybe Mark, Int )


type alias BoardState =
    Array (Maybe Mark)



-- MODEL


type alias Model =
    { activePlayer : Maybe Mark
    , turn : Int
    , boardState : BoardState
    , winner : Maybe Mark
    , showModal : Bool
    }


initialModel : Model
initialModel =
    { activePlayer = Nothing
    , turn = 0
    , boardState = Array.repeat 9 Nothing

    -- near terminal state used for testing
    -- , boardState = fromList [ Just O, Nothing, Just X, Just X, Nothing, Just X, Nothing, Just O, Just O ]
    , winner = Nothing
    , showModal = True
    }



-- UPDATE


type Msg
    = MarkCell Play
    | ResetBoard
    | ChooseMark Mark


fullRow : BoardState -> Maybe Mark
fullRow rowSlice =
    let
        rowList =
            rowSlice |> toList
    in
        if List.all isX rowList then
            Just X
        else if List.all isO rowList then
            Just O
        else
            Nothing



-- sets mark then returns updated board state


setMark : Play -> Model -> Model
setMark play model =
    { model
        | boardState =
            model.boardState
                |> Array.set (second play) (first play)
        , activePlayer = nextPlayer (maybe (first play))
    }



-- used to flip the active player to the inverse of the mark param


nextPlayer : Mark -> Maybe Mark
nextPlayer mark =
    case mark of
        X ->
            Just O

        O ->
            Just X


toggleModal : BoardState -> Maybe Mark -> Bool
toggleModal board winner =
    case winner of
        Just X ->
            True

        Just O ->
            True

        _ ->
            False


checkWin : BoardState -> Maybe Mark -> Bool
checkWin boardState mark =
    let
        boardTuple =
            ( slice 0 3 boardState, slice 3 6 boardState, slice 6 9 boardState )

        ( topRow, midRow, bottomRow ) =
            boardTuple

        firstCol =
            Array.map maybe (fromList [ get 0 topRow, get 0 midRow, get 0 bottomRow ])

        secondCol =
            Array.map maybe (fromList [ get 1 topRow, get 1 midRow, get 1 bottomRow ])

        thirdCol =
            Array.map maybe (fromList [ get 2 topRow, get 2 midRow, get 2 bottomRow ])

        backSlash =
            Array.map maybe (fromList [ get 0 topRow, get 1 midRow, get 2 bottomRow ])

        forwardSlash =
            Array.map maybe (fromList [ get 2 topRow, get 1 midRow, get 0 bottomRow ])
    in
        if fullRow topRow == mark || fullRow midRow == mark || fullRow bottomRow == mark then
            True
        else if fullRow firstCol == mark || fullRow secondCol == mark || fullRow thirdCol == mark then
            True
        else if fullRow backSlash == mark || fullRow forwardSlash == mark then
            True
        else
            False


update : Msg -> Model -> Model
update message model =
    let
        { activePlayer, boardState, turn } =
            model
    in
        case message of
            MarkCell ( mark, index ) ->
                let
                    nextGameState =
                        setMark ( mark, index ) model |> .boardState

                    theWinner =
                        if checkWin nextGameState mark then
                            mark
                        else
                            Nothing
                in
                    case mark of
                        Just O ->
                            { model
                                | boardState = nextGameState
                                , turn = turn + 1
                                , activePlayer = Just X
                                , winner = theWinner
                                , showModal = toggleModal nextGameState theWinner
                            }

                        Just X ->
                            { model
                                | boardState = nextGameState
                                , turn = turn + 1
                                , activePlayer = Just O
                                , winner = theWinner
                                , showModal = toggleModal nextGameState theWinner
                            }

                        _ ->
                            { model | activePlayer = Nothing }

            ChooseMark mark ->
                { model
                    | activePlayer = Just mark
                    , showModal = False
                }

            ResetBoard ->
                initialModel



-- VIEW


renderMark : BoardState -> Int -> String
renderMark boardState index =
    case getMark boardState index of
        Just X ->
            "X"

        Just O ->
            "O"

        _ ->
            ""


renderWinner : Maybe Mark -> String
renderWinner winner =
    case winner of
        Just X ->
            "X"

        Just O ->
            "O"

        _ ->
            "Nobody"


view : Model -> Html Msg
view model =
    let
        { boardState, activePlayer, turn, winner } =
            model

        { showModal } =
            model

        cell index =
            button <|
                el Cell
                    [ disabled <|
                        if getMark boardState index == Nothing then
                            False
                        else
                            True
                    , width (px 150)
                    , height (px 150)
                    , onClick (MarkCell ( activePlayer, index ))
                    ]
                    (renderMark boardState index |> text)

        chooseMarkBtn mark =
            button <|
                el Button
                    [ disabled <|
                        if turn > 0 then
                            True
                        else
                            False
                    , onClick (ChooseMark mark)
                    ]
                    (mark |> toString |> text)
    in
        Element.viewport MyStyles.stylesheet <|
            el None
                [ height (fill 100), width (fill 100), center ]
                (column Main
                    [ center, spacingXY 0 10, height (fill 100), width (fill 100) ]
                    [ header <| el Label [ center ] (text "Elm Tic-Tac-Toe")
                    , when showModal
                        (column Modal
                            [ width (percent 100), height (percent 100), center, verticalCenter ]
                            [ when (turn == 0)
                                (column None
                                    [ spacingXY 20 20 ]
                                    [ row None
                                        [ spacing 20, center ]
                                        [ el None [] (text "Whom goes first?")
                                        ]
                                    , row None
                                        [ center, spacingXY 10 10 ]
                                        [ chooseMarkBtn X
                                        , chooseMarkBtn O
                                        ]
                                    ]
                                )

                            -- game results
                            , when (not (winner == Nothing) || turn == 9)
                                (column None
                                    [ width (percent 50), spacingXY 0 20 ]
                                    [ el None [ center ] (text "The winner is: ")
                                    , el None [ center ] (winner |> renderWinner |> text)
                                    , button <| el Button [ onClick ResetBoard, width (percent 50), center ] (text "Reset Game")
                                    ]
                                )
                            ]
                        )
                    , when (not showModal)
                        (section <|
                            column None
                                [ spacingXY 0 10, center, verticalCenter ]
                                [ row None
                                    [ spacingXY 10 0 ]
                                    [ cell 0
                                    , cell 1
                                    , cell 2
                                    ]
                                , row None
                                    [ spacingXY 10 0 ]
                                    [ cell 3
                                    , cell 4
                                    , cell 5
                                    ]
                                , row None
                                    [ spacingXY 10 0 ]
                                    [ cell 6
                                    , cell 7
                                    , cell 8
                                    ]
                                ]
                        )
                    , screen <|
                        column Footer
                            [ alignBottom, padding 10, width (percent 100), center ]
                            [ el None
                                []
                                (text "All of the code for this game was written in Elm")
                            , link
                                "https://github.com/chrstntdd/elm-tic-tac-toe"
                              <|
                                el Link [ center ] (text "Check it out here")
                            ]
                    ]
                )
