module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)



---- MODEL ----


type alias Model =
    { weatherResults : List WeatherItem
    , errorMessage : Maybe String
    , locationQuery : String
    }


type alias WeatherItem =
    { dt_txt : String
    , temps : MainInfo
    , descriptionList : List WeatherDescription
    }


type alias MainInfo =
    { temp_min : Maybe Float
    , temp_max : Maybe Float
    }


type alias WeatherDescription =
    { description : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { weatherResults = [], errorMessage = Nothing, locationQuery = "6324729" }, searchWeather "6324729" )



---- UPDATE ----


type Msg
    = SetLocationQuery String
    | SearchWeather
    | WeatherResult (Result Http.Error (List WeatherItem))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetLocationQuery locationQuery ->
            ( { model | locationQuery = locationQuery }, Cmd.none )

        SearchWeather ->
            ( model, searchWeather model.locationQuery )

        WeatherResult (Ok results) ->
            ( { model | weatherResults = results, locationQuery = "6324729" }, Cmd.none )

        WeatherResult (Err err) ->
            ( { model | weatherResults = [], errorMessage = Just "Oops, something went wrong." }, Cmd.none )



--TODO API KEY GOES IN HERE


searchWeather : String -> Cmd Msg
searchWeather locationQuery =
    let
        url =
            "https://api.openweathermap.org/data/2.5/forecast?id="
                ++ locationQuery
                ++ "&appid=INSERTAPIKEYHERE&units=metric"
    in
    Http.send WeatherResult <|
        Http.get url decodeWeatherList



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "Five day Forecast for Halifax, NS" ]
        , div
            [ class "forecastGroup" ]
            (List.map
                weatherItem
                (List.map changeDate model.weatherResults)
            )
        ]


weatherItem : WeatherItem -> Html Msg
weatherItem weather =
    div [ class "forecastItem" ]
        [ p [ class "date" ] [ text weather.dt_txt ]
        , p [ class "date" ] [ text (String.dropLeft 11 weather.dt_txt) ]
        , p
            [ class "high" ]
            [ text ("High: " ++ String.fromFloat (Maybe.withDefault 0.0 weather.temps.temp_max)) ]
        , p [ class "low" ]
            [ text ("Low: " ++ String.fromFloat (Maybe.withDefault 0.0 weather.temps.temp_min)) ]
        , div
            []
            (List.map viewWeatherList weather.descriptionList)
        ]


changeDate : WeatherItem -> WeatherItem
changeDate weather =
    { weather | dt_txt = String.dropRight 9 weather.dt_txt }


viewWeatherList : WeatherDescription -> Html Msg
viewWeatherList description =
    p [ class "description" ] [ text (Maybe.withDefault "Not Available" description.description) ]


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage errorMessage =
    case errorMessage of
        Just message ->
            h1 [ class "error-message" ] [ text message ]

        Nothing ->
            text ""


decodeWeatherList : Json.Decode.Decoder (List WeatherItem)
decodeWeatherList =
    Json.Decode.at [ "list" ] (Json.Decode.list decodeWeatherItem)


decodeMain : Json.Decode.Decoder MainInfo
decodeMain =
    Json.Decode.succeed MainInfo
        |> Json.Decode.Pipeline.optional "temp_min" (Json.Decode.map Just float) Nothing
        |> Json.Decode.Pipeline.optional "temp_max" (Json.Decode.map Just float) Nothing


decodeWeatherItem : Json.Decode.Decoder WeatherItem
decodeWeatherItem =
    Json.Decode.succeed WeatherItem
        |> Json.Decode.Pipeline.required "dt_txt" Json.Decode.string
        |> Json.Decode.Pipeline.required "main" decodeMain
        |> Json.Decode.Pipeline.required "weather" (Json.Decode.list decodeWeatherInfo)


decodeWeatherInfo : Json.Decode.Decoder WeatherDescription
decodeWeatherInfo =
    Json.Decode.succeed WeatherDescription
        |> Json.Decode.Pipeline.optional "description" (Json.Decode.map Just string) Nothing



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
