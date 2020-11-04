module Pages.CreateEvent.Date_Int exposing (Model, Msg, Params, page)

import Element exposing (..)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Time exposing (Posix)


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
    { date : Posix }


init : Url Params -> ( Model, Cmd Msg )
init { params } =
    ( { date = Time.millisToPosix params.date }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Create Event"
    , body =
        [ model.date
            |> Time.posixToMillis
            |> String.fromInt
            |> text
        ]
    }
