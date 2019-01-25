module Pomodoro exposing (PomodoroLineItem, main, updateEstimate, viewErrors)

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

import Browser exposing (element)
import Dict
import Html exposing (Attribute, Html, button, div, input, label, li, ol, text)
import Html.Attributes exposing (disabled, for, id, style)
import Html.Events exposing (onClick, onInput)
import List as Lst
import String as Str exposing (fromInt)


type alias PomodoroLineItem =
    { taskName : String
    , estimated : Int
    , actual : Int
    , position : Int
    }


type alias PomodoroList =
    List PomodoroLineItem


type alias Model =
    { list : PomodoroList
    , latest : PomodoroLineItem
    , errors : Dict.Dict String (List String)
    }


type Msg
    = InputTask String String
    | InputEstimate String String
    | AddTask


newPomodoroLineItem : PomodoroLineItem
newPomodoroLineItem =
    PomodoroLineItem "" 0 0 0


initModel : Model
initModel =
    { list = []
    , latest = newPomodoroLineItem
    , errors = Dict.empty
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel, Cmd.none )


main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


addTask : Model -> ( Model, Cmd Msg )
addTask mod =
    let
        latest =
            mod.latest
    in
    ( { mod
        | list = List.append mod.list [ latest ]
        , latest = newPomodoroLineItem
      }
    , Cmd.none
    )


updateInputTask : String -> String -> Model -> ( Model, Cmd Msg )
updateInputTask _ value mod =
    let
        latest =
            mod.latest

        updated =
            { mod
                | latest =
                    { latest | taskName = value }
            }
    in
    ( updated, Cmd.none )


updateEstimate : String -> String -> Model -> ( Model, Cmd Msg )
updateEstimate fieldName value mod =
    let
        latest =
            mod.latest

        result =
            if Str.isEmpty value then
                { mod
                    | errors = Dict.empty
                    , latest = { latest | estimated = 0 }
                }

            else
                case Str.toInt value of
                    Just i ->
                        { mod
                            | latest = { latest | estimated = i }
                            , errors = Dict.empty
                        }

                    Nothing ->
                        { mod
                            | errors =
                                Dict.singleton
                                    fieldName
                                    [ "Not a number" ]
                            , latest = { latest | estimated = 0 }
                        }
    in
    ( result, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mod =
    case msg of
        InputTask fieldName value ->
            updateInputTask fieldName value mod

        InputEstimate fieldName value ->
            updateEstimate fieldName value mod

        AddTask ->
            addTask mod



-- Views


globalFontStyle : Attribute Msg
globalFontStyle =
    style "font-family" "sans-serif"


errorFontStyle : Attribute Msg
errorFontStyle =
    style "color" "red"


type alias Rule =
    String -> Validation


type alias Validation =
    { isReady : Bool
    , errors : Dict.Dict String (List String)
    }


viewErrors : String -> Dict.Dict String (List String) -> Html Msg
viewErrors idString dict =
    let
        errorMaybe =
            Dict.get idString dict

        result =
            case errorMaybe of
                Just errs ->
                    ol [ id <| idString ++ "-errors", errorFontStyle ]
                        (List.map (\s -> li [] [ text s ]) errs)

                Nothing ->
                    text ""
    in
    result


viewField : String -> String -> Model -> (String -> String -> Msg) -> Html Msg
viewField idString labelString model function =
    div [ id <| idString ++ "-field" ]
        [ label [ for idString ] [ text labelString ]
        , input [ id idString, onInput <| function idString ] []
        , viewErrors idString model.errors
        ]


viewButton : Model -> Html Msg
viewButton mod =
    let
        latest =
            mod.latest

        taskNameBlank =
            latest.taskName |> String.isEmpty

        estimatedZero =
            latest.estimated |> (==) 0

        isDisabled =
            taskNameBlank || estimatedZero
    in
    button [ onClick AddTask, disabled isDisabled ] [ text "Add Task" ]
        |> Debug.log "isDisabled"


view : Model -> Html Msg
view mod =
    div [ id "outer", globalFontStyle ]
        [ div [ id "pomo-list" ]
            [ text "List of Pomodoros"
            , ol [ id "ordered-pomo-list" ]
                (Lst.map
                    (\item ->
                        li []
                            [ text <|
                                item.taskName
                                    ++ " "
                                    ++ fromInt
                                        item.estimated
                            ]
                    )
                    mod.list
                )
            ]
        , div [ id "pomo-entry" ]
            [ viewField "task-input" "Task Name:" mod InputTask
            , viewField "task-estimate" "Estimated:" mod InputEstimate
            , viewButton mod
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
