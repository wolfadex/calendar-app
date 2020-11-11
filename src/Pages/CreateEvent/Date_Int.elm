module Pages.CreateEvent.Date_Int exposing (Model, Msg, Params, page)

import Api.Event as Event exposing (Event)
import Browser.Navigation as Nav
import Element exposing (..)
import Form.Field exposing (Field)
import Gui
import Gui.Form.Field
import Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Time exposing (Posix)
import Update.Pipeline as Up


page : Page Params Model Msg
page =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Params =
    { date : Int }


type alias Model =
    { date : Posix
    , name : Field String
    , customerNote : Field String
    , createRequest : WebData ()
    , navKey : Nav.Key
    }


init : Url Params -> ( Model, Cmd Msg )
init { params, key } =
    ( { date = Time.millisToPosix params.date
      , name = Form.Field.init { value = "", parser = Event.parseName }
      , customerNote = Form.Field.init { value = "", parser = Event.parseCustomerNote }
      , createRequest = NotAsked
      , navKey = key
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = CreateEvent
    | CreateEventResponse (WebData ())
    | GotName String
    | GotCustomerNotes String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotName newName ->
            case model.createRequest of
                NotAsked ->
                    updateName newName model

                Failure _ ->
                    updateName newName model

                _ ->
                    Up.save model

        GotCustomerNotes newCustomerNotes ->
            case model.createRequest of
                NotAsked ->
                    updateCustomerNote newCustomerNotes model

                Failure _ ->
                    updateCustomerNote newCustomerNotes model

                _ ->
                    Up.save model

        CreateEvent ->
            case model.createRequest of
                NotAsked ->
                    createEvent model

                Failure _ ->
                    createEvent model

                _ ->
                    Up.save model

        CreateEventResponse response ->
            case model.createRequest of
                Loading ->
                    ( { model | createRequest = response }
                    , case response of
                        Success () ->
                            Nav.pushUrl model.navKey (Route.toString Route.Calendar)

                        _ ->
                            Cmd.none
                    )

                _ ->
                    Up.save model


updateName : String -> Model -> ( Model, Cmd Msg )
updateName newName model =
    Up.save { model | name = Form.Field.update newName model.name }


updateCustomerNote : String -> Model -> ( Model, Cmd Msg )
updateCustomerNote newCustomerNotes model =
    Up.save { model | customerNote = Form.Field.update newCustomerNotes model.customerNote }


createEvent : Model -> ( Model, Cmd Msg )
createEvent model =
    case Event.parse model of
        Ok event ->
            ( { model | createRequest = Loading }
            , RemoteData.Http.post
                "/api/event"
                CreateEventResponse
                decodeCreateRequest
                (Event.encodeNew event)
            )

        Err err ->
            Up.save model


decodeCreateRequest : Decoder ()
decodeCreateRequest =
    Json.Decode.succeed ()


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    let
        maybeFormError =
            [ model.name
                |> Form.Field.toActual
                |> resultToErrorMaybe
            , model.customerNote
                |> Form.Field.toActual
                |> resultToErrorMaybe
            ]
                |> List.filterMap identity
                |> List.head
    in
    { title = "Create Event"
    , body =
        [ column
            [ spacing 16 ]
            [ Gui.Form.Field.text
                { label = text "Name"
                , onChange = GotName
                , value = model.name
                }
            , Gui.Form.Field.multiline
                { label = text "Customer Notes"
                , onChange = GotCustomerNotes
                , value = model.customerNote
                }
            , row
                [ width fill ]
                [ link
                    []
                    { label = text "Cancel"
                    , url = Route.toString Route.Calendar
                    }
                , Gui.button
                    { label =
                        text <|
                            case model.createRequest of
                                NotAsked ->
                                    "Create"

                                Failure _ ->
                                    "Create"

                                Success () ->
                                    "Created"

                                Loading ->
                                    "Creating..."
                    , onPress =
                        case maybeFormError of
                            Nothing ->
                                Just CreateEvent

                            Just _ ->
                                Nothing
                    }
                    |> el [ alignRight ]
                ]
            ]
        ]
    }


resultToErrorMaybe : Result e a -> Maybe e
resultToErrorMaybe result =
    case result of
        Ok _ ->
            Nothing

        Err err ->
            Just err
