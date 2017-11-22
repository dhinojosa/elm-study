
module Pomodoro exposing (main)

import Html exposing (Html, text, div, ol, li, button, input, img)
import List exposing (map, append, singleton)
import Html.Attributes exposing (id, value, src, placeholder)
import Html.Events exposing (onClick, onInput)
import Tuple exposing (second, first)
import Time
import Debug exposing (log)

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

view : Model -> Html Msg
view mod = text("Hello World")

subscriptions : Model -> Sub Msg
subscriptions mod = (Sub.none)


