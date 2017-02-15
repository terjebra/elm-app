module Utils exposing (..)

import Date exposing (Date)


formatTime : Date -> String
formatTime date =
    (toString (Date.hour date) |> addPadding) ++ ":" ++ (toString (Date.minute date) |> addPadding) ++ ":" ++ (toString (Date.second date) |> addPadding)


addPadding : String -> String
addPadding string =
    if String.length string == 1 then
        "0" ++ string
    else
        string


average : Int -> Float -> Float
average len sum =
    sum / toFloat (len)
