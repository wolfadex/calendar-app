module Api.Database exposing (mutate)

import Http exposing (Error(..), Response(..))
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Task exposing (Task)


mutate : String -> Decoder a -> Task Error a
mutate mutation decoder =
    Http.task
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/graphql"
        , body =
            [ ( "query"
              , "mutation {"
                    ++ mutation
                    ++ "}"
                    |> Json.Encode.string
              )
            ]
                |> Json.Encode.object
                |> Http.jsonBody
        , resolver =
            Http.stringResolver
                (\resp ->
                    case resp of
                        BadUrl_ url ->
                            Err (BadUrl url)

                        Timeout_ ->
                            Err Timeout

                        NetworkError_ ->
                            Err NetworkError

                        BadStatus_ metadata body ->
                            Err (BadStatus metadata.statusCode)

                        GoodStatus_ metadata body ->
                            Json.Decode.decodeString decoder body
                                |> Result.mapError (Json.Decode.errorToString >> BadBody)
                )
        , timeout = Nothing
        }
