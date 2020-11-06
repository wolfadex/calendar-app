module Api exposing (handler)

import Api.Event as Event
import Date
import Error
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Logger as Log
import Response
import Server exposing (Config, Flags, Method(..), Path, Request, Response)
import Status exposing (Status(..))


handler : Request -> Path -> Response
handler request path =
    case path of
        [ "event" ] ->
            case Server.getMethod request of
                Get ->
                    []
                        |> Json.Encode.list Event.encode
                        |> Response.json
                        |> Server.respond request

                Post ->
                    case Server.decodeBody Event.decodeNew request of
                        Ok _ ->
                            Response.ok
                                |> Response.setStatus Created
                                |> Server.respond request

                        Err err ->
                            Response.error (Json.Decode.errorToString err)
                                |> Response.setStatus BadRequest
                                |> Server.respond request

                _ ->
                    Server.respond request Response.methodNotAllowed

        _ ->
            Server.respond request Response.notFound
