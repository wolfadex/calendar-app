module Api.Event exposing
    ( Event
    , decode
    , decodeNew
    , encode
    , encodeNew
    , parse
    , parseCustomerNote
    , parseName
    )

import Form.Field exposing (Field)
import Html.Attributes exposing (name)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Uuid exposing (Uuid)


type Event
    = Event Internal


type alias Internal =
    { name : String
    , customerNote : String
    }


parseName : String -> Result String String
parseName name =
    if String.isEmpty name then
        Err "A customer name is required"

    else
        Ok name


parseCustomerNote : String -> Result String String
parseCustomerNote note =
    Ok note


parse : { r | name : Field String, customerNote : Field String } -> Result String Event
parse fields =
    Result.map2
        (\name customerNote ->
            Event { name = name, customerNote = customerNote }
        )
        (fields.name |> Form.Field.toActual)
        (fields.customerNote |> Form.Field.toActual)


encode : ( Uuid, Event ) -> Value
encode ( id, Event { name, customerNote } ) =
    Json.Encode.object
        [ ( "name", Json.Encode.string name )
        , ( "customerNote", Json.Encode.string customerNote )
        , ( "id", Uuid.encode id )
        ]


encodeNew : Event -> Value
encodeNew (Event { name, customerNote }) =
    Json.Encode.object
        [ ( "name", Json.Encode.string name )
        , ( "customerNote", Json.Encode.string customerNote )
        ]


decode : Decoder ( Uuid, Event )
decode =
    Json.Decode.map3
        (\id name customerNote ->
            ( id
            , Event
                { name = name
                , customerNote = customerNote
                }
            )
        )
        (Json.Decode.field "id" Uuid.decode)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "customerNote" Json.Decode.string)


decodeNew : Decoder Event
decodeNew =
    Json.Decode.map2
        (\name customerNote ->
            Event
                { name = name
                , customerNote = customerNote
                }
        )
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "customerNote" Json.Decode.string)
