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
