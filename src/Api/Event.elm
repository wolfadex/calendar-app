module Api.Event exposing
    ( Error(..)
    , Event
    , decode
    , decodeNew
    , encode
    , encodeNew
    , parse
    )

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Uuid exposing (Uuid)


type Error
    = NameRequired


type Event
    = Event Internal


type alias Internal =
    { name : String
    , customerNote : String
    }


parse : { name : String, customerNote : String } -> Result Error Event
parse { name, customerNote } =
    if String.isEmpty name then
        Err NameRequired

    else
        Ok (Event { name = name, customerNote = customerNote })


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
