module Temperatures exposing (..)

import Temperature
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Html.Events exposing (onClick)


--MODEL


type alias Model =
    { temperatures : List Temperature.Temperature
    , alertMessage : Maybe String
    , temperatureInput : String
    , timeInput : String
    }


initialModel : Model
initialModel =
    { temperatures =
        []
    , alertMessage = Nothing
    , temperatureInput = ""
    , timeInput = ""
    }



--UPDATE


type Msg
    = NewTemperatures (Result Http.Error (List Temperature.Temperature))
    | NewTemperature (Result Http.Error Temperature.Temperature)
    | SetTemperatureInput String
    | SetTimeInput String
    | SaveTemperature
    | ClearTemperature
    | CloseAlert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTemperatures (Ok temperatures) ->
            ( { model | temperatures = temperatures }, Cmd.none )

        NewTemperatures (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        NewTemperature (Ok temperature) ->
            let
                message =
                    "Temperature reading registered"
            in
                ( { model | alertMessage = Just message, temperatures = temperature :: model.temperatures }, Cmd.none )

        NewTemperature (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        SetTemperatureInput temp ->
            ( { model | temperatureInput = temp }, Cmd.none )

        SetTimeInput time ->
            ( { model | timeInput = time }, Cmd.none )

        SaveTemperature ->
            ( { model | timeInput = "", temperatureInput = "" }, postTemperature model.timeInput (Result.withDefault 0 (String.toFloat model.temperatureInput)) )

        ClearTemperature ->
            ( { model | timeInput = "", temperatureInput = "" }, Cmd.none )


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
    case error of
        Http.NetworkError ->
            "Could not connect to Server"

        Http.BadStatus response ->
            (toString response.status)

        Http.BadPayload message _ ->
            "Decoding failed: " ++ message

        _ ->
            (toString error)



--Commands


temperaturesUrl : String
temperaturesUrl =
    "http://localhost:3000/temperatures"


getTemperatures : Cmd Msg
getTemperatures =
    Temperature.getTemperatures NewTemperatures temperaturesUrl


postTemperature : String -> Float -> Cmd Msg
postTemperature time temp =
    Temperature.postTemperature NewTemperature temperaturesUrl time temp



--VIEW


viewSection : Model -> Html Msg
viewSection model =
    let
        titleInfoText =
            "Todays temperature readings"
                |> text
    in
        section []
            [ h2 [ id "temperature-title" ] [ titleInfoText ]
            , Temperature.viewTemperatureInput SaveTemperature ClearTemperature SetTemperatureInput SetTimeInput model.temperatureInput model.timeInput
            , Temperature.viewAverage model.temperatures
            , Temperature.viewTemperatureList model.temperatures
            ]


viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert", onClick CloseAlert ]
                [ text message ]

        Nothing ->
            text ""


sectionText : List Temperature.Temperature -> String
sectionText temperatures =
    List.length temperatures
        |> toString
        |> String.append "Number of readings: "


viewHeader : String -> Html Msg
viewHeader title =
    Html.header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "https://www.yr.no" ]
            [ text "Yr.no" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewAlertMessage model.alertMessage
        , viewHeader "Temperatures"
        , viewSection model
        , viewFooter
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getTemperatures )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
