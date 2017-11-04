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

module Main exposing (..)

import Html exposing (Html, input, text, 
                      div, br, beginnerProgram)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput)
import String exposing (uncons, fromChar, toLower, split, concat)
import List exposing (member, map, intersperse)

main  : Program Never String Action
main  = beginnerProgram {model = model,
                         view  = view,
                         update = update}

type Action = Keyed String

model : String
model = "" 

isVowel : Char -> Bool
isVowel c = member c ['a', 'e', 'i', 'o', 'u']

pigLatinize   : String -> String
pigLatinize s = case uncons << toLower <| s of
                   Just (h, t) -> 
                       case isVowel h of
                          Fals
              div [] [input [onInput Keyed] []],
              div [] [text mod]
            ]

update : Action -> String -> String
update act mod = case act of 
                    Keyed s -> concat << intersperse " " 
                                      << map pigLatinize 
                                      << split " " <| s
