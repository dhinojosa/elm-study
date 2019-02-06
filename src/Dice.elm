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


module Dice exposing (main)

import Html exposing (Html, text)
import Browser exposing (element)
import String exposing (fromInt)

main : Program () Model msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model
    = { pip : Int }


init : flags -> ( Model, Cmd msg )
init _ =
    ( Model 1 , Cmd.none )


update : msg -> Model -> ( Model, Cmd msg )
update _ md =
    ( md, Cmd.none )


view : Model -> Html msg
view m =
    text <| "Current Pip:" ++ (fromInt <| m.pip)


subscriptions : Model -> Sub msg
subscriptions =
    always Sub.none
