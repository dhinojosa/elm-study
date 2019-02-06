module HttpCall exposing (main)

import Browser exposing (element)
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (Decoder, list, decodeString, int, dict, string)


type alias Model =
    { lookup : String, result : String }


type Action
    = OnEnter String
    | OnSubmit
    | OnGet (Result Http.Error String)


init : () -> ( Model, Cmd msg )
init flg =
    ( { lookup = "", result = "" }, Cmd.none )


view : Model -> Html Action
view mod =
    div [ id "outer-div" ]
        [ h1 []
            [ text "Enter the Country Name and Get the Currency"
            ]
        , text "Enter Country:"
        , input
            [ id "submit-div"
            , onInput OnEnter
            ]
            []
        , div [ id "result-div" ]
            [ text ("Currency Result:" ++ mod.result)
            ]
        ]


subscriptions : Model -> Sub msg
subscriptions mod =
    Sub.none


contentDecoder : Decoder String
contentDecoder = string


update : Action -> Model -> ( Model, Cmd Action )
update act mod =
    case act of
        OnEnter s ->
            ( mod
            , Http.get
                { url = ""
                , expect = Http.expectJson OnGet contentDecoder
                }
            )

        _ ->
            ( mod, Cmd.none )

type alias Currency =
    { primary : String
    , secondary : String
    }


type alias Iso =
    { code2 : String
    , code3 : String
    , num : String
    }


type alias Languages =
    List String


type alias Record =
    { currency : Currency
    , iso : Iso
    , languages : Languages
    , name : String
    , region : String
    }


main : Program () Model Action
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
