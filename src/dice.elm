
import Random as Rnd
import Html exposing (Html, text)

main : Program Never Model msg
main = Html.program {
         init = init,
         update = update,
         subscriptions = subscriptions,
         view = view
       }

type Model = Roll {pip:Int}

init : (Model, Cmd msg)
init = (Roll 1, Cmd.none)

update : msg -> Model -> (Model, Cmd msg)
update mg md = (md, Cmd.none)

view : Model -> Html msg
view m = text ("Current Pip:" ++ (toString << pip <| m))

subscriptions : Model -> Sub msg
subscriptions = always Sub.none
