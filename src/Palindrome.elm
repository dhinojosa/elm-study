module Palindrome exposing (main, palindrome)

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

import Html exposing (Html, text, div, input)
import Html.Events exposing (onInput)
import String exposing (reverse)

type alias Model = String

type LatestWord = LatestWord String

model : Model
model = ""

palindrome : String -> String
palindrome s = reverse s

main : Program Never Model LatestWord
main = Html.beginnerProgram {
          model = model,
          view = view,
          update = update
       }

view : Model -> Html LatestWord
view mod = div[] [
               text "Enter word:",
               input[onInput LatestWord][],
               text mod
            ]

update : LatestWord -> Model -> Model
update ms mod = case ms of
                  LatestWord s -> palindrome s
