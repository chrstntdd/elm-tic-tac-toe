module Main exposing (..)

import Array exposing (get)
import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Element.Events exposing (onClick)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition
import String exposing (length, contains)


-- Util functions


getMarkAt : Array.Array String -> Int -> String
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


isMarked : Array.Array String -> Int -> Bool
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


type Styles
    = None
    | Main
    | Box
    | Label


type Mark
    = X
    | O


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
    , boardState : Array.Array String
    , selectedMark : String
    , turn : Int
    , winner : String
    }


initialModel : Board
initialModel =
    { activePlayer = ""
    , boardState = Array.fromList [ "", "", "", "", "", "", "", "", "" ]
    , selectedMark = ""
    , turn = 0
    , winner = ""
    }



-- UPDATE


type Msg
    = MarkCell Int String
    | ResetBoard
    | ChooseMark String



-- | DisplayOutcome


update : Msg -> Board -> Board
update message board =
    let
        { activePlayer, boardState, turn } =
            board
    in
        case message of
            MarkCell index mark ->
                if contains mark "O" && length activePlayer == 1 then
                    { board
                        | boardState = Array.set index mark boardState
                        , turn = turn + 1
                        , activePlayer = "X"
                    }
                else if contains mark "X" && length activePlayer == 1 then
                    { board
                        | boardState = Array.set index mark boardState
                        , turn = turn + 1
                        , activePlayer = "O"
                    }
                else
                    { board | activePlayer = "" }

            ChooseMark mark ->
                { board | activePlayer = mark }

            ResetBoard ->
                initialModel


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None [] -- It's handy to have a blank style
        , style Main
            [ Border.all 1 -- set all border widths to 1 px.
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Color.border Color.lightGrey
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            ]
        , style Label
            [ Font.size 25 -- set font size to 25 px
            , Font.center
            ]
        , style Box
            [ Transition.all
            , Color.text Color.white
            , Color.background Color.blue
            , Color.border Color.blue
            , Border.rounded 3 -- round all borders to 3px
            , paddingHint 20
            , hover
                [ Color.text Color.white
                , Color.background Color.red
                , Color.border Color.red
                , cursor "pointer"
                ]
            ]
        ]


{-| Our view is made up of `Element`s,
which you can think of as Html with layout, positioning, and spacing built in.
-}
view : Board -> Html Msg
view board =
    let
        { boardState, activePlayer, turn } =
            board

        cell index =
            button <|
                el Box
                    [ disabled <| isMarked boardState index
                    , width (px 200)
                    , height (px 200)
                    , onClick (MarkCell index activePlayer)
                    ]
                <|
                    text (getMarkAt boardState index)

        chooseMarkBtn mark =
            button <|
                el Box
                    [ disabled <|
                        if turn > 0 then
                            True
                        else
                            False
                    , onClick (ChooseMark mark)
                    ]
                    (text mark)
    in
        Element.root stylesheet <|
            column Main
                [ center, width (px 800), spacing 50, paddingTop 50, paddingBottom 50 ]
                [ el Label [] (text "Elm Tic-Tac-Toe")
                , el Label [] (text (toString board))
                , chooseMarkBtn "X"
                , chooseMarkBtn "O"
                , button <| el Box [ onClick ResetBoard ] (text "Reset Game")
                , wrappedRow None
                    [ spacingXY 10 10, center ]
                    [ cell 0
                    , cell 1
                    , cell 2
                    , cell 3
                    , cell 4
                    , cell 5
                    , cell 6
                    , cell 7
                    , cell 8
                    ]
                ]
