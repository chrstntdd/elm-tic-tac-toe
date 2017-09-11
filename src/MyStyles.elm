module MyStyles exposing (..)

import Color exposing (rgba)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition


type Styles
    = None
    | Main
    | Box
    | Label
    | Modal
    | Cell
    | Footer
    | Button
    | Link


colors =
    { fountainBlue = rgba 95 180 203 1
    , cometPurple = rgba 89 98 119 1
    , atlantisGreen = rgba 126 208 59 1
    , webOrange = rgba 239 172 0 1
    , offWhite = rgba 242 242 242 1
    }


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None [] -- It's handy to have a blank style
        , style Main
            [ Border.all 1 -- set all border widths to 1 px.
            , Color.text Color.darkCharcoal
            , Color.background colors.offWhite
            , Color.border Color.lightGrey
            , Font.typeface [ "helvetica" ]
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
        , style Modal
            [ Transition.all
            , Color.text Color.black
            , Color.background colors.fountainBlue
            , Color.border Color.grey
            , Style.prop "z-index" "1"
            , Style.prop "position" "absolute"
            ]
        , style Cell
            [ Transition.all
            , Color.text colors.cometPurple
            , Color.background colors.fountainBlue
            , Color.border colors.atlantisGreen
            , Font.size 26
            , cursor "pointer"
            ]
        , style Footer
            [ Transition.all
            , Color.background colors.atlantisGreen
            , Font.typeface [ "helvetica" ]
            , Font.size 16
            ]
        , style Button
            [ Transition.all
            , Color.background colors.webOrange
            , Border.rounded 3
            , Color.text Color.white
            , Color.border colors.webOrange
            , paddingHint 20
            , hover
                [ Color.text colors.atlantisGreen
                ]
            ]
        , style Link
            [ Transition.all
            , Color.text colors.cometPurple
            ]
        ]
