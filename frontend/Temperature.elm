module Temperature
    exposing
        ( Temperature
        , postTemperature
        , getTemperatures
        , viewTemperatureList
        , viewTemperatureItem
        , viewAverage
        , viewTemperatureInput
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode exposing (..)
import Utils exposing (..)
import Html.Events exposing (onClick, onInput)


type alias Temperature =
    { id : Int
    , time : String
    , reading : Float
    }


temperatureDecoder : Decoder Temperature
temperatureDecoder =
    Decode.map3 Temperature
        (field "id" Decode.int)
        (field "time" Decode.string)
        (field "reading" Decode.float)


encodeReading : String -> Float -> Encode.Value
encodeReading time temp =
    Encode.object
        [ ( "time", Encode.string time )
        , ( "reading", Encode.float temp )
        ]


postTemperature : (Result Http.Error Temperature -> msg) -> String -> String -> Float -> Cmd msg
postTemperature msg url time temp =
    let
        body =
            encodeReading time temp
                |> Http.jsonBody

        request =
            Http.post url body temperatureDecoder
    in
        Http.send msg request


getTemperatures : (Result Http.Error (List Temperature) -> msg) -> String -> Cmd msg
getTemperatures msg url =
    (Decode.list temperatureDecoder)
        |> Http.get url
        |> Http.send msg


viewTemperatureList : List Temperature -> Html msg
viewTemperatureList temperatures =
    let
        listOfTemperatures =
            List.map viewTemperatureItem temperatures
    in
        ul [] listOfTemperatures


viewTemperatureItem : Temperature -> Html msg
viewTemperatureItem temperature =
    li []
        [ span [ class "time" ] [ text temperature.time ]
        , span [ class "temp" ] [ text (toString temperature.reading) ]
        ]


averageTemperature : List Temperature -> Float
averageTemperature temperatures =
    temperatures
        |> List.map .reading
        |> List.sum
        |> average (List.length temperatures)


viewAverage : List Temperature -> Html msg
viewAverage temperatures =
    div
        [ class "average" ]
        [ span [ class "label" ] [ text "Average " ]
        , span [ class "average" ] [ text (toString (averageTemperature temperatures)) ]
        ]


viewTemperatureInput : msg -> msg -> (String -> msg) -> (String -> msg) -> String -> String -> Html msg
viewTemperatureInput saveMsg cancelMsg setTempInput setTimeInput temperatureInput timeInput =
    div []
        [ input
            [ type_ "text"
            , placeholder "Temperature"
            , value temperatureInput
            , onInput setTempInput
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Time"
            , value timeInput
            , onInput setTimeInput
            ]
            []
        , button [ onClick saveMsg ] [ text "Save" ]
        , button [ onClick cancelMsg ] [ text "Cancel" ]
        ]
