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


module Main exposing
    ( Model
    , OnInputEvent(..)
    , appendToString
    , capFirst
    , consPigLatin
    , isLowerCaseConsonant
    , isLowerCaseVowel
    , main
    , model
    , pigLatin
    , update
    , view
    )

import Browser exposing (sandbox)
import Char
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onInput)
import List exposing (member)
import String exposing (append, cons, reverse, toLower, uncons)


type alias Model =
    String


model : Model
model =
    ""


type OnInputEvent
    = OnInputEvent String


main : Program () Model OnInputEvent
main =
    sandbox
        { init = model
        , view = view
        , update = update
        }


view : Model -> Html OnInputEvent
view mod =
    div [ id "outer-div" ]
        [ text "Enter word:"
        , input [ onInput (\s -> OnInputEvent s) ] []
        , text mod
        ]


appendToString : String -> Char -> String
appendToString s c =
    reverse (cons c (reverse s))


isLowerCaseVowel : Char -> Bool
isLowerCaseVowel c =
    member c [ 'a', 'e', 'i', 'o', 'u' ]


isLowerCaseConsonant : Char -> Bool
isLowerCaseConsonant c =
    not << isLowerCaseVowel <| c


capFirst : String -> String
capFirst s =
    case uncons s of
        Just ( h, t ) ->
            cons (Char.toUpper h) t

        Nothing ->
            ""


consPigLatin : String -> String -> String
consPigLatin acc word =
    case uncons word of
        Just ( h, t ) ->
            if isLowerCaseConsonant h then
                consPigLatin (appendToString acc h) t

            else
                word ++ acc ++ "ay"

        Nothing ->
            ""


pigLatin : String -> String
pigLatin s =
    case uncons << toLower <| s of
        Just ( h, t ) ->
            if isLowerCaseVowel h then
                cons h t ++ "way"

            else
                consPigLatin "" s

        Nothing ->
            ""


update : OnInputEvent -> Model -> Model
update evt mod =
    case evt of
        OnInputEvent s ->
            capFirst << pigLatin <| s
