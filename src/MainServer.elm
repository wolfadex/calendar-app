module MainServer exposing (main)

import Api
import Error
import Iso8601
import Json.Decode exposing (Decoder)
import Json.Encode
import Logger as Log
import Response
import Server exposing (Config, Flags, Method(..), Request, Response)
import Status exposing (Status(..))
import Task
import Time
import TimeZone exposing (Error(..))


main : Server.Program
main =
    Server.program
        { init = init
        , handler = handler
        }


init : Flags -> Config
init { arguments } =
    let
        port_ =
            case Json.Decode.decodeValue decodeArguments arguments of
                Err _ ->
                    8001

                Ok p ->
                    p
    in
    Server.baseConfig
        |> Server.withPort port_


decodeArguments : Decoder Int
decodeArguments =
    Json.Decode.oneOf
        [ Json.Decode.field "--port" Json.Decode.int
        , Json.Decode.field "-p" Json.Decode.int
        ]


handler : Request -> Response
handler request =
    logRequest request
        |> Server.andThen
            (\_ ->
                handleCors request <|
                    \_ ->
                        case Server.getPath request of
                            [] ->
                                Response.ok
                                    |> Response.setBody "Hello, Server!"
                                    |> Server.respond request

                            "api" :: rest ->
                                Api.handler request rest

                            _ ->
                                Server.respond request Response.notFound
            )


handleCors : Request -> (Request -> Response) -> Response
handleCors request handle =
    case Server.getMethod request of
        Options ->
            Response.ok
                |> Response.setStatus NoContent
                |> Response.addHeader "Access-Control-Allow-Origin" "*"
                |> Response.addHeader "Access-Control-Allow-Methods" "OPTIONS, POST, GET"
                |> Response.addHeader "Access-Control-Allow-Headers" "X-Requested-With, Accept, Accept-Encoding, Accept-Language, Cache-Control, Connection, Content-Length, Content-Type, Host, Origin, Pragma, Referer, User-Agent"
                |> Response.addHeader "Access-Control-Max-Age" "2592000"
                |> Server.respond request

        _ ->
            handle request


logRequest : Request -> Response
logRequest request =
    let
        path =
            Server.getPath request

        method =
            Server.getMethod request
    in
    Task.map
        (\time ->
            Iso8601.fromTime time
                ++ " "
                ++ Server.methodToString method
                ++ " "
                ++ String.join "/" path
        )
        Time.now
        |> Task.mapError (\_ -> Error.fromString "Server error")
        |> Task.andThen Log.toConsole
