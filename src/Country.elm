module Country exposing (main)

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

import Browser exposing (element)
import Debug exposing (log)
import Html exposing (Html, br, button, div, input, text)
import Html.Attributes exposing (id, placeholder)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder, at, field, list, map2, map4)
import Maybe
--import Regex exposing (fromString, replace)
import Result exposing (Result(..))
import String exposing (fromInt)
import Url.Builder exposing (QueryParameter, crossOrigin, string)
import String
import List


type alias Country =
    { name : String
    , nativeName : String
    , currencyName : String
    , flagPng : String
    }


type alias Model =
    { entry : String, result : List Country, error : String }


type Action
    = OnEnter String
    | OnClick
    | OnGet (Result Http.Error (List Country))


getURL : String -> String
getURL country =
    crossOrigin
        "http://countryapi.gear.host"
        [ "v1", "Country", "getCountries" ]
        [ string "pName" country ]


countryList : Decoder (List Country)
countryList =
    let
        nameField =
            field "Name" Decode.string

        nativeNameField =
            field "NativeName" Decode.string

        currencyNameField =
            field "CurrencyName" Decode.string

        flagPngField =
            field "FlagPng" Decode.string

        countryDecoder =
            map4 Country
                nameField
                nativeNameField
                currencyNameField
                flagPngField
    in
    field "Response" (list countryDecoder)


getCountryInfo : String -> Cmd Action
getCountryInfo s =
    Http.get
        { url = getURL s
        , expect = Http.expectJson OnGet countryList
        }


init : () -> ( Model, Cmd Action )
init _ =
    ( { entry = "", result = [], error = "" }, Cmd.none )

view : Model -> Html Action
view mod =
    div [ id "outer-div" ]
        [ div [ id "entry-div" ]
            [ text "Enter name of the country"
            , input [ id "country-name", onInput OnEnter ] []
            , button [ onClick OnClick ] [ text "Submit" ]
            ]
        , div [ id "result-div" ]
            [
               text <| "Given:" ++ mod.entry
               ,text <| "Currency:" ++ (String.join "," (List.map .currencyName mod.result))
               ,text <| "Error:" ++ mod.error
            ]
        ]


update : Action -> Model -> ( Model, Cmd Action )
update act mod =
    case act of
        OnEnter s ->
            ( { mod | entry = s }, Cmd.none )

        OnClick ->
            ( mod, getCountryInfo mod.entry )

        OnGet (Err (BadUrl s)) ->
            ( { mod | error = "Bad URL:" ++ s }, Cmd.none )

        OnGet (Err Timeout) ->
            ( { mod | error = "Timeout" }, Cmd.none )

        OnGet (Err NetworkError) ->
            ( { mod | error = "Network Error" }, Cmd.none )

        OnGet (Err (BadStatus i)) ->
            ( { mod | error = "Bad Status:" ++ fromInt i }, Cmd.none )

        OnGet (Err (BadBody s)) ->
            ( { mod | error = "Bad Body:" ++ s }, Cmd.none )

        OnGet (Ok []) ->
            ( { mod | entry = "", error = "No content" }, Cmd.none )

        OnGet (Ok xs) ->
            ( { mod | entry = "", error = "", result = xs }, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions mod =
    Sub.none


main : Program () Model Action
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
