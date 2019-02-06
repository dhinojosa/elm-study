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

port module MyPorts exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick, onInput)
import Browser exposing (element)

main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { query : String
    , answer : String
    }


type Msg
    = Click
    | Answer String
    | OnInput String


subscriptions : Model -> Sub Msg
subscriptions _ = fromJs Answer

view : Model -> Html Msg
view mod =
    div [ id "outer" ]
        [ div [ id "send" ]
            [ input [ onInput OnInput ] []
            , button [ id "sendJs", onClick Click ]
                [ text "Send Now" ]
            ]
        , div [ id "get" ]
            [ text <| mod.answer
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mod =
    case msg of
        Click ->
            ( mod, toJs mod.query )

        OnInput s ->
            ( { mod | query = s }, Cmd.none )

        Answer s ->
            ( { mod | answer = s }, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "", Cmd.none )


port toJs : String -> Cmd msg

port fromJs : (String -> msg) -> Sub msg
