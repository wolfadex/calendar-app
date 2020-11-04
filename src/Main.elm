module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes exposing (rows)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Task
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra exposing (Interval(..))
import TimeZone


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Loading
    | Loaded
        { currentTime : Posix
        , viewingTime : Posix
        , timeZone : { name : String, zone : Zone }
        }
    | FailedToLoad TimeZone.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Task.map2
        (\now ( name, zone ) ->
            ( name, zone, now )
        )
        Time.now
        TimeZone.getZone
        |> Task.attempt GotTimeAndZone
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = GotTimeAndZone (Result TimeZone.Error ( String, Zone, Posix ))
    | ShowPreviousMonth
    | ShowNextMonth


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotTimeAndZone result, Loading ) ->
            case result of
                Ok ( name, zone, time ) ->
                    ( Loaded
                        { currentTime = time
                        , viewingTime = time
                        , timeZone = { name = name, zone = zone }
                        }
                    , Cmd.none
                    )

                Err err ->
                    ( FailedToLoad err, Cmd.none )

        ( ShowPreviousMonth, Loaded data ) ->
            ( Loaded
                { data | viewingTime = Time.Extra.add Month -1 data.timeZone.zone data.viewingTime }
            , Cmd.none
            )

        ( ShowNextMonth, Loaded data ) ->
            ( Loaded
                { data | viewingTime = Time.Extra.add Month 1 data.timeZone.zone data.viewingTime }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = " Let's Meet"
    , body =
        [ layout [ width fill, height fill ] (viewBody model)
        ]
    }


viewBody : Model -> Element Msg
viewBody model =
    case model of
        Loading ->
            text "Loading..."

        FailedToLoad err ->
            column
                []
                [ text "Failed to load with error:"
                , case err of
                    TimeZone.NoZoneName ->
                        text "No zone name"

                    TimeZone.NoDataForZoneName name ->
                        text ("No data for zone " ++ name)
                ]

        Loaded { currentTime, viewingTime, timeZone } ->
            let
                { zone } =
                    timeZone

                { year, month } =
                    Time.Extra.posixToParts zone viewingTime

                firstDayOfMonth =
                    Time.Extra.floor Month zone viewingTime

                firstOfWeek =
                    Time.Extra.floor Week zone firstDayOfMonth

                secondWeek =
                    Time.Extra.add Week 1 zone firstOfWeek

                thirdWeek =
                    Time.Extra.add Week 1 zone secondWeek

                fourthWeek =
                    Time.Extra.add Week 1 zone thirdWeek

                lastDayOfTheMonth =
                    Time.Extra.ceiling Month zone viewingTime
                        |> Time.Extra.add Day -1 zone

                lastWeek =
                    Time.Extra.floor Week zone lastDayOfTheMonth

                secondToLastWeek =
                    Time.Extra.add Week -1 zone lastWeek

                viewWeekHelper =
                    viewWeek
                        { zone = zone
                        , currentTime = currentTime
                        , viewingTime = viewingTime
                        }
            in
            column
                []
                [ row
                    [ width fill
                    , spacing 16
                    ]
                    [ Input.button
                        []
                        { label = text "<-"
                        , onPress = Just ShowPreviousMonth
                        }
                    , monthToString month
                        ++ " "
                        ++ String.fromInt year
                        |> text
                        |> el [ width fill ]
                    , Input.button
                        []
                        { label = text "->"
                        , onPress = Just ShowNextMonth
                        }
                    ]
                , viewWeekHelper firstOfWeek
                , viewWeekHelper secondWeek
                , viewWeekHelper thirdWeek
                , viewWeekHelper fourthWeek
                , if secondToLastWeek == fourthWeek || secondToLastWeek == thirdWeek then
                    none

                  else
                    viewWeekHelper secondToLastWeek
                , if lastWeek == fourthWeek then
                    none

                  else
                    viewWeekHelper lastWeek
                ]


viewWeek : { zone : Zone, currentTime : Posix, viewingTime : Posix } -> Posix -> Element Msg
viewWeek ({ zone } as args) firstDayOfWeek =
    List.range 1 7
        |> List.foldl
            (\_ ( thisDay, acc ) ->
                ( Time.Extra.add Day 1 zone thisDay
                , thisDay :: acc
                )
            )
            ( firstDayOfWeek, [] )
        |> Tuple.second
        |> List.reverse
        |> List.map (viewDay args)
        |> row []


viewDay : { zone : Zone, currentTime : Posix, viewingTime : Posix } -> Posix -> Element Msg
viewDay { zone, currentTime, viewingTime } calendateDate =
    let
        isToday =
            Time.Extra.floor Day zone calendateDate
                == Time.Extra.floor Day zone currentTime

        isNotCurrentMonth =
            Time.Extra.floor Month zone calendateDate
                /= Time.Extra.floor Month zone viewingTime
    in
    calendateDate
        |> Time.toDay zone
        |> String.fromInt
        |> text
        |> el
            [ width (px 80)
            , height (px 80)
            , padding 4
            , Border.solid
            , Border.width 1
            , Font.color <|
                if isToday then
                    rgb 1 0 0

                else if isNotCurrentMonth then
                    rgb 0.75 0.75 0.75

                else
                    rgb 0 0 0
            ]


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"
