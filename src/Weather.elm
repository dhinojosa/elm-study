module Weather exposing (constructURL, main)

-- Copyright (c) 2017 Hinojosa, Daniel <dhinojosa@evolutionnext.com>
-- Author: Hinojosa, Daniel <dhinojosa@evolutionnext.com>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy of
-- this software and associated documentation files (the "Software"), to deal in
-- the Software without restriction, including without limitation the rights to
-- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
-- the Software, and to permit persons to whom the Software is furnished to do so,
-- subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
-- FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
-- COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
-- IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import Browser
import Html exposing (Html, br, button, div, input, text)
import Html.Attributes exposing (id, placeholder)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, at, map2)
import Maybe
import Regex exposing (fromString, replace)
import String
import Url.Builder exposing (crossOrigin)

--completeString : String
--completeString = """https://gist.githubusercontent.com/dhinojosa/877425fb98a939a816e2c56f02bbedd0/raw/84158bf19d58dca011cecf267eae0307232b76f3/countries.json"""
--
--completeURL : String
--completeURL = crossOrigin
--                 "https://gist.githubusercontent.com"
--                 ["dhinojosa"
--                 ,"877425fb98a939a816e2c56f02bbedd0"
--                 ,"ram"
--                 ,"84158bf19d58dca011cecf267eae0307232b76f3"
--                 ,"countries.json"]
--                 []

type alias Model =
    { cityState : String
    , condition : Condition
    , error : String
    }

type alias Condition =
    { description : String
    , temperature : Maybe Int
    }

type Msg
    = OnClick
    | OnInput String
    | NewRequest (Result Error Condition)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    Tuple.pair
        { cityState = ""
        , condition =
            { description = ""
            , temperature = Nothing
            }
        , error = ""
        }
        Cmd.none


baseQuery : String
baseQuery =
    "select * from weather.forecast where woeid in (select woeid from geo.places(1) where text=\"%fill-city%\")"


baseUrl : String
baseUrl =
    "https://query.yahooapis.com/v1/public/yql?q="


constructURL : String -> String
constructURL cityState =
    let
        maybe =
            fromString "%fill-city%"

        regex =
            Maybe.withDefault Regex.never maybe
    in
    replace regex (\_ -> cityState) baseQuery


suffixURL : String
suffixURL =
    "&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"


completeURL : String -> String
completeURL cityState =
    String.concat [ baseUrl, constructURL cityState, suffixURL ]


viewTemp : Model -> Html Msg
viewTemp mod =
    case mod.condition.temperature of
        Just n ->
            text (String.fromInt n ++ "°F")

        Nothing ->
            text ""


view : Model -> Html Msg
view mod =
    div [ id "weather-accumulator" ]
        [ div [ id "weather-form" ]
            [ input [ placeholder "City, State", onInput OnInput ] []
            , button [ id "submit", onClick OnClick ] [ text "Enter" ]
            ]
        , div [ id "weather-panel" ]
            [ text << .description << .condition <| mod
            , br [] []
            , viewTemp mod
            ]
        , div [ id "error-panel" ]
            [ text << .error <| mod ]
        ]


stringToMaybeIntDecoder : Decoder (Maybe Int)
stringToMaybeIntDecoder =
    Decode.map String.toInt Decode.string


decodeCondition : Decoder Condition
decodeCondition =
    map2
        Condition
        (at [ "query", "results", "channel", "item", "condition", "text" ] Decode.string)
        (at [ "query", "results", "channel", "item", "condition", "temp" ] stringToMaybeIntDecoder)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mod =
    case msg of
        OnInput s ->
            ( { mod | cityState = s }, Cmd.none )

        OnClick ->
            ( mod
            , Http.get
                { url = completeURL <| mod.cityState
                , expect = Http.expectJson NewRequest decodeCondition
                }
            )

        NewRequest (Ok cond) ->
            ( { mod | condition = cond }, Cmd.none )

        NewRequest (Err (BadUrl s)) ->
            ( { mod | error = s }, Cmd.none )

        NewRequest (Err Timeout) ->
            ( { mod | error = "Timeout" }, Cmd.none )

        NewRequest (Err NetworkError) ->
            ( { mod | error = "Network Error" }, Cmd.none )

        NewRequest (Err (BadStatus s)) ->
            ( { mod | error = "Bad Status" ++ (String.fromInt s) }, Cmd.none )

        NewRequest (Err (BadBody s)) ->
            ( { mod | error = "Bad Body: " ++ s }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions mod =
    Sub.none
