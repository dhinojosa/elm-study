
module Pomodoro exposing (main)

import Html exposing (Html, Attribute, text, div, label, input, button, ol, li)
import Html.Attributes exposing (id, for, style)
import List exposing (map)

type alias PomodoroLineItem =
   {taskName : String,
    estimated : Int,
    actual : Int, 
    position: Int}

type alias PomodoroList = List PomodoroLineItem

type alias Model =
   {pomodoroList : PomodoroList,
    pomodoroLineItem: PomodoroLineItem}

type Msg = AddTask

newPomodoroLineItem : PomodoroLineItem
newPomodoroLineItem = PomodoroLineItem "New Task" 1 0 0

initModel : Model
initModel = {pomodoroList = [],
             pomodoroLineItem = newPomodoroLineItem}

init : (Model, Cmd msg)
init = (initModel, Cmd.none)

main : Program Never Model Msg
main = Html.program {
          init = init,
          view = view,
          update = update,
          subscriptions = subscriptions
       }

update : Msg -> Model -> (Model, Cmd Msg)
update msg mod = (mod, Cmd.none)


-- Views

globalFontStyle : Attribute Msg
globalFontStyle = style [
                    ("font-family", "sans-serif")
                  ]

view : Model -> Html Msg
view mod = div[id "outer", globalFontStyle] [
             div [id "pomo-list"] [
                text "List of Pomodoros",
                ol [id "ordered-pomo-list"] 
                     (map (\item -> li[][text item.taskName]) mod.pomodoroList)
             ],
             div [id "pomo-entry"] [
                label[for "task-input"][text "Task Name:"],
                input[id "task-input"][],
                label[for "estimate-input"][text "Estimated:"],
                input[id "estimate-input"][],
                button[][text "Add"]
             ]
           ]

subscriptions : Model -> Sub Msg
subscriptions mod = (Sub.none)
