-- Copyright 2017 Hinojosa, Daniel <dhinojosa@evolutionnext.com>
-- Author: Hinojosa, Daniel <dhinojosa@evolutionnext.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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
