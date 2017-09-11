module Main exposing (..)

import Array exposing (Array, filter, fromList, get, length, map, set, slice, toList)
import Color exposing (rgba)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Html exposing (Html, footer)
import MyStyles exposing (..)
import String exposing (contains, length)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition


-- Util functions


getMarkAt : Array String -> Int -> String
getMarkAt array index =
    case get index array of
        Just "X" ->
            "X"

        Just "O" ->
            "O"

        Nothing ->
            ""

        _ ->
            ""


isMarked : Array String -> Int -> Bool
isMarked array index =
    case get index array of
        Just "X" ->
            True

        Just "O" ->
            True

        Just "" ->
            False

        Nothing ->
            True

        _ ->
            True


isX : String -> Bool
isX mark =
    if mark == "X" then
        True
    else
        False


isO : String -> Bool
isO mark =
    if mark == "O" then
        True
    else
        False


maybe : Maybe a -> a
maybe maybeA =
    case maybeA of
        Just a ->
            a

        Nothing ->
            Debug.crash "Can't unwrap that maybe"


main : Program Never Board Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }



-- MODEL


type alias Board =
    { activePlayer : String
    , boardState : Array String
    , selectedMark : String
    , turn : Int
    , winner : String
    , showModal : Bool
    }


initialModel : Board
initialModel =
    { activePlayer = ""
    , boardState = fromList [ "", "", "", "", "", "", "", "", "" ]
    , selectedMark = ""
    , turn = 0
    , winner = ""
    , showModal = True
    }



-- UPDATE


type Msg
    = MarkCell Int String
    | ResetBoard
    | ChooseMark String


fullRow : Array String -> String
fullRow rowSlice =
    let
        rowOfX =
            filter isX rowSlice

        rowOfO =
            filter isO rowSlice
    in
    if Array.length rowOfX == 3 then
        "X"
    else if Array.length rowOfO == 3 then
        "O"
    else
        ""


toggleModal : Array String -> String -> Bool
toggleModal boardState winner =
    if Array.length (Array.filter (\mark -> mark == "") boardState) == 0 then
        True
    else if winner == "X" || winner == "O" || winner == "Nobody" then
        True
    else
        False


checkWin : Array String -> String
checkWin boardState =
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
    if fullRow topRow == "X" || fullRow topRow == "O" then
        fullRow topRow
    else if fullRow midRow == "X" || fullRow midRow == "O" then
        fullRow midRow
    else if fullRow bottomRow == "X" || fullRow bottomRow == "O" then
        fullRow bottomRow
    else if fullRow firstCol == "X" || fullRow firstCol == "O" then
        fullRow firstCol
    else if fullRow secondCol == "X" || fullRow secondCol == "O" then
        fullRow secondCol
    else if fullRow thirdCol == "X" || fullRow thirdCol == "O" then
        fullRow thirdCol
    else if fullRow backSlash == "X" || fullRow backSlash == "O" then
        fullRow backSlash
    else if fullRow forwardSlash == "X" || fullRow forwardSlash == "O" then
        fullRow forwardSlash
    else if Array.length (Array.filter (\mark -> mark == "") boardState) == 0 then
        "Nobody!"
    else
        ""


update : Msg -> Board -> Board
update message board =
    let
        { activePlayer, boardState, turn } =
            board
    in
    case message of
        MarkCell index mark ->
            let
                nextBoardState index mark boardState =
                    set index mark boardState

                theWinner =
                    checkWin <| nextBoardState index mark boardState
            in
            if contains mark "O" && String.length activePlayer == 1 then
                { board
                    | boardState = nextBoardState index mark boardState
                    , turn = turn + 1
                    , activePlayer = "X"
                    , winner = theWinner
                    , showModal = toggleModal (nextBoardState index mark boardState) theWinner
                }
            else if contains mark "X" && String.length activePlayer == 1 then
                { board
                    | boardState = nextBoardState index mark boardState
                    , turn = turn + 1
                    , activePlayer = "O"
                    , winner = theWinner
                    , showModal = toggleModal (nextBoardState index mark boardState) theWinner
                }
            else
                { board | activePlayer = "" }

        ChooseMark mark ->
            { board
                | activePlayer = mark
                , showModal = False
            }

        ResetBoard ->
            initialModel


{-| Our view is made up of `Element`s,
which you can think of as Html with layout, positioning, and spacing built in.
-}
view : Board -> Html Msg
view board =
    let
        { boardState, activePlayer, turn, winner, showModal } =
            board

        cell index =
            button <|
                el Cell
                    [ disabled <| isMarked boardState index || not (String.isEmpty winner) || activePlayer == ""
                    , width (px 150)
                    , height (px 150)
                    , onClick (MarkCell index activePlayer)
                    ]
                    (getMarkAt boardState index |> text)

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
                    (mark |> text)
    in
    Element.viewport MyStyles.stylesheet <|
        column Main
            [ center, spacingXY 0 10, height (percent 100), width (fill 100) ]
            [ header <| el Label [] (text "Elm Tic-Tac-Toe")
            , when showModal
                (column Modal
                    [ width (percent 80), height (percent 60), center, verticalCenter ]
                    [ when (turn == 0)
                        (column None
                            [ spacingXY 20 20 ]
                            [ row None
                                [ spacing 20, center ]
                                [ el None [] (text "Whom goes first?")
                                ]
                            , row None
                                [ center, spacingXY 10 10 ]
                                [ chooseMarkBtn "X"
                                , chooseMarkBtn "O"
                                ]
                            ]
                        )
                    , when (not (String.isEmpty winner))
                        (column None
                            [ width (percent 50) ]
                            [ el None [ center ] (text "The winner is")
                            , el None [ center ] (text winner)
                            , button <| el Button [ onClick ResetBoard, width (percent 25), center ] (text "Reset Game")
                            ]
                        )
                    ]
                )
            , section <|
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
