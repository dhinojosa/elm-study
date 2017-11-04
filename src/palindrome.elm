
import Html exposing (Html, text, div, input)
import Html.Events exposing (onInput)
import String exposing (reverse)

type alias Model = String

type LatestWord = LatestWord String

model : Model
model = ""

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
                  LatestWord s -> reverse s
