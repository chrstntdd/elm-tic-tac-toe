module Main exposing (..)

import Array
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


-- Util functions


isEven : Int -> Bool
isEven n =
    n % 2 == 0


getMarkAt : Array.Array String -> Int -> String
getMarkAt array index =
    toString <| Array.get index array


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
    case message of
        MarkCell index mark ->
            if isEven board.turn then
                { board
                    | boardState = Array.set index mark board.boardState
                    , turn = board.turn + 1
                    , activePlayer = "O"
                }
            else
                { board
                    | boardState = Array.set index mark board.boardState
                    , turn = board.turn + 1
                    , activePlayer = "X"
                }

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
        { boardState } =
            board
    in
        Element.root stylesheet <|
            column Main
                [ center, width (px 800), spacing 50, paddingTop 50, paddingBottom 50 ]
                [ el Label [] (text "Elm Tic-Tac-Toe")
                , el Label [] (text (toString board))
                , button <| el Box [ onClick (ChooseMark "X") ] (text "X")
                , button <| el Box [ onClick (ChooseMark "O") ] (text "O")
                , button <| el Box [ onClick ResetBoard ] (text "reset board")
                , wrappedRow None
                    [ spacingXY 10 10, center ]
                    [ button <| el Box [ width (px 200), height (px 200), onClick (MarkCell 0 "X") ] <| text <| getMarkAt boardState 0
                    , button <| el Box [ width (px 200), height (px 200), onClick (MarkCell 1 "O") ] <| text <| getMarkAt boardState 1
                    , button <| el Box [ width (px 200), height (px 200), onClick (MarkCell 2 "O") ] <| text <| getMarkAt boardState 2
                    , button <| el Box [ width (px 200), height (px 200), onClick (MarkCell 3 "O") ] <| text <| getMarkAt boardState 3
                    , button <| el Box [ width (px 200), height (px 200), onClick (MarkCell 4 "O") ] <| text <| getMarkAt boardState 4
                    , button <| el Box [ width (px 200), height (px 200), onClick (MarkCell 5 "O") ] <| text <| getMarkAt boardState 5
                    , button <| el Box [ width (px 200), height (px 200), onClick (MarkCell 6 "O") ] <| text <| getMarkAt boardState 6
                    , button <| el Box [ width (px 200), height (px 200), onClick (MarkCell 7 "O") ] <| text <| getMarkAt boardState 7
                    , button <| el Box [ width (px 200), height (px 200), onClick (MarkCell 8 "O") ] <| text <| getMarkAt boardState 8
                    ]
                ]
