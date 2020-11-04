module Gui exposing (button)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input


button : { label : Element msg, onPress : Maybe msg } -> Element msg
button =
    Input.button
        [ Border.solid
        , Border.width 3
        , paddingXY 16 8
        ]
