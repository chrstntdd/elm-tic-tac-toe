module Main exposing (..)

import Array exposing (Array, filter, fromList, get, length, map, set, slice)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Html exposing (Html, footer)
import MyStyles exposing (..)


-- Util functions


getMarkAt : Array Mark -> Int -> Mark
getMarkAt array index =
    case get index array of
        Just X ->
            X

        Just O ->
            O

        Nothing ->
            Empty

        _ ->
            Empty


isMarked : Array Mark -> Int -> Bool
isMarked array index =
    case get index array of
        Just X ->
            True

        Just O ->
            True

        Just Empty ->
            False

        Nothing ->
            True


isX : Mark -> Bool
isX mark =
    if mark == X then
        True
    else
        False


isO : Mark -> Bool
isO mark =
    if mark == O then
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



-- MODEL


type Mark
    = X
    | O
    | Empty


main : Program Never Board Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }



-- MODEL


type alias Board =
    { activePlayer : Mark
    , humanPlayer : Mark
    , aiPlayer = Mark
    , boardState : Array Mark
    , turn : Int
    , winner : Mark
    , showModal : Bool
    }


initialModel : Board
initialModel =
    { activePlayer = Empty
    , humanPlayer = O
    , aiPlayer = X
    -- , boardState = Array.repeat 9 Empty
    , boardState = fromList [O, Empty, X, X, Empty, X, Empty, O, O]
    , turn = 0
    , winner = Empty
    , showModal = True
    }



-- UPDATE


type Msg
    = MarkCell Int Mark
    | ResetBoard
    | ChooseMark Mark


fullRow : Array Mark -> Mark
fullRow rowSlice =
    let
        rowOfX =
            filter isX rowSlice

        rowOfO =
            filter isO rowSlice
    in
    if Array.length rowOfX == 3 then
        X
    else if Array.length rowOfO == 3 then
        O
    else
        Empty


emptyIndexes : Array Mark -> List Int
emptyIndexes boardState =
    Array.filter (\mark -> mark == Empty) boardState


minimax boardState mark =
    let
        availSpots = emptyIndexes boardState
    in
        if winning newBoard huPlayer then
            {score = -10}
        else if winning newBoard aiPlayer then
            {score = 10}
        else if availSpots == 0 then
            {score = 0}

    

        {-
         map over all empty spots
         for each empty spot, create a move record
        -}




toggleModal : Array Mark -> Mark -> Bool
toggleModal boardState winner =
    if emptyIndexes boardState == 0 then
        True
    else if winner == X || winner == O then
        True
    else
        False


checkWin : Array Mark -> Mark
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
    if fullRow topRow == X || fullRow topRow == O then
        fullRow topRow
    else if fullRow midRow == X || fullRow midRow == O then
        fullRow midRow
    else if fullRow bottomRow == X || fullRow bottomRow == O then
        fullRow bottomRow
    else if fullRow firstCol == X || fullRow firstCol == O then
        fullRow firstCol
    else if fullRow secondCol == X || fullRow secondCol == O then
        fullRow secondCol
    else if fullRow thirdCol == X || fullRow thirdCol == O then
        fullRow thirdCol
    else if fullRow backSlash == X || fullRow backSlash == O then
        fullRow backSlash
    else if fullRow forwardSlash == X || fullRow forwardSlash == O then
        fullRow forwardSlash
    else if Array.length (Array.filter (\mark -> mark == Empty) boardState) == 0 then
        Empty
    else
        Empty


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
            if mark == O && not (activePlayer == Empty) then
                { board
                    | boardState = nextBoardState index mark boardState
                    , turn = turn + 1
                    , activePlayer = X
                    , winner = theWinner
                    , showModal = toggleModal (nextBoardState index mark boardState) theWinner
                }
            else if mark == X && not (activePlayer == Empty) then
                { board
                    | boardState = nextBoardState index mark boardState
                    , turn = turn + 1
                    , activePlayer = O
                    , winner = theWinner
                    , showModal = toggleModal (nextBoardState index mark boardState) theWinner
                }
            else
                { board | activePlayer = Empty }

        ChooseMark mark ->
            { board
                | activePlayer = mark
                | humanPlayer = mark
                , showModal = False
            }

        ResetBoard ->
            initialModel


renderMark : Array Mark -> Int -> String
renderMark boardState index =
    if getMarkAt boardState index == Empty then
        ""
    else
        getMarkAt boardState index |> toString


renderWinner : Mark -> String
renderWinner winner =
    if winner == Empty then
        "Nobody!"
    else
        winner |> toString


view : Board -> Html Msg
view board =
    let
        { boardState, activePlayer, turn, winner, showModal } =
            board

        cell index =
            button <|
                el Cell
                    [ disabled <| isMarked boardState index || not (winner == Empty) || activePlayer == Empty
                    , width (px 150)
                    , height (px 150)
                    , onClick (MarkCell index activePlayer)
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
                        , when (not (winner == Empty) || turn == 9)
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
