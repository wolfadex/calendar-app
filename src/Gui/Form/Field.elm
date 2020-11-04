module Gui.Form.Field exposing (multiline, text)

import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Form.Field exposing (Field)


text : { value : Field a, onChange : String -> msg, label : Element msg } -> Element msg
text { value, onChange, label } =
    Input.text
        []
        { label = Input.labelLeft [] label
        , onChange = onChange
        , placeholder = Nothing
        , text = Form.Field.toRaw value
        }
        |> fieldWithError value


multiline : { value : Field a, onChange : String -> msg, label : Element msg } -> Element msg
multiline { value, onChange, label } =
    Input.multiline
        []
        { label = Input.labelAbove [] label
        , onChange = onChange
        , placeholder = Nothing
        , text = Form.Field.toRaw value
        , spellcheck = True
        }
        |> fieldWithError value


fieldWithError : Field a -> Element msg -> Element msg
fieldWithError field input =
    Element.column
        [ Element.width Element.fill ]
        [ input
        , case Form.Field.toActual field of
            Ok _ ->
                Element.none

            Err err ->
                err
                    |> Element.text
                    |> Element.el [ Font.color (Element.rgb 1 0 0) ]
        ]
