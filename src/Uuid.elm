module Uuid exposing (Uuid, decode, encode)

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type Uuid
    = Uuid String


encode : Uuid -> Value
encode (Uuid id) =
    Json.Encode.string id


decode : Decoder Uuid
decode =
    Json.Decode.map Uuid
        Json.Decode.string
