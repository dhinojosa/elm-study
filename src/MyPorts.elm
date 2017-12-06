port module MyPorts exposing (..)

import Html exposing (Html, button, div, input, program, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick, onInput)

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { query : String
    , answer : String
    }


type Msg
    = Click
    | Answer String
    | OnInput String


subscriptions : Model -> Sub Msg
subscriptions mod =
    fromJs Answer


view : Model -> Html Msg
view mod =
    div [ id "outer" ]
        [ div [ id "send" ]
            [ input [ onInput OnInput ] []
            , button [ id "sendJs", onClick Click ]
                [ text "Send Now" ]
            ]
        , div [ id "get" ]
            [ text <| mod.answer
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mod =
    case msg of
        Click ->
            ( mod, toJs mod.query )

        OnInput s ->
            ( { mod | query = s }, Cmd.none )

        Answer s ->
            ( { mod | answer = s }, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( Model "" "", Cmd.none )


port toJs : String -> Cmd msg


port fromJs : (String -> msg) -> Sub msg
