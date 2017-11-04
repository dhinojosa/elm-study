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

import Html exposing (Html,text, div, ol, li, button,input)
import List exposing (map)
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput)
import Tuple exposing (second, first)

type alias TextFieldString = String
type alias ListOfTasks = List String
type alias Model = (TextFieldString, ListOfTasks)
type Action = AddTask | EnterTask String

model : Model
model = ("", [])

main : Program Never Model Action
main = Html.beginnerProgram {
          model = model,
          view = view,
          update = update
       }

view : Model -> Html Action
view mod = let fieldData = first mod
               taskList = second mod in
                   div [] [
                      div [] [text "List of Items"],
                      ol [] (map (\i -> li[][text i]) taskList),
                      div [id "field-div"] [
                        input [id "newTask", onInput EnterTask, value fieldData] [],
                        button [onClick AddTask] [text "Click to Add"]
                      ]
                   ]

update : Action -> Model -> Model
update msg prevState = let fieldData = first prevState
                           taskList = second prevState in
                             case msg of
                               EnterTask s -> (s, taskList)
                               AddTask     -> ("", taskList ++ [fieldData])
