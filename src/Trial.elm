module Trail exposing (main)

import Browser exposing (element)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onInput)
import String

main : Program () Model MyEvent
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    String


type MyEvent
    = ReverseString String

init : () -> ( Model, Cmd msg )
init _ =
    ( "", Cmd.none )


view : Model -> Html MyEvent
view md =
    div [ id "outer" ]
        [ div []
            [ text "Hello, Elm" ]
        , div []
            [ text "Reverse Word:"
            , input [ id "reverse-word", onInput ReverseString ] []
            , text md
            ]
        ]

update : MyEvent -> Model -> ( Model, Cmd MyEvent )
update mg _ =
    case mg of
        ReverseString s ->
            ( String.reverse s, Cmd.none )

subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
