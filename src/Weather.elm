module Weather exposing (constructURL, main)

import Debug exposing (log)
import Html exposing (Html, br, button, div, input, text)
import Html.Attributes exposing (id, placeholder)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error, send)
import Json.Decode as Decode exposing (Decoder, at, decodeString, map2)
import Regex exposing (HowMany(All), regex, replace)
import String
import Time


type alias Model =
    { cityState : String
    , condition : Condition
    , error : String
    , time: Float}


type alias Condition =
    { description : String
    , temperature : Maybe Int
    }


type Msg
    = OnClick
    | OnInput String
    | NewRequest (Result Error Condition)
    | Tick Time.Time


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { cityState = "", condition = { description = "", temperature = Nothing }, error = "", time = 0.0 }, Cmd.none )


baseQuery : String
baseQuery =
    "select * from weather.forecast where woeid in (select woeid from geo.places(1) where text=\"%fill-city%\")"


baseUrl : String
baseUrl =
    "https://query.yahooapis.com/v1/public/yql?q="


constructURL : String -> String
constructURL cityState =
    replace All (regex "%fill-city%") (\_ -> cityState) baseQuery


suffixURL : String
suffixURL =
    "&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"


viewTemp : Model -> Html Msg
viewTemp mod =
    case mod.condition.temperature of
        Just n ->
            text (toString n ++ "°F")

        Nothing ->
            text ""


view : Model -> Html Msg
view mod =
    div [ id "weather-accumulator" ]
        [ div [ id "weather-form" ]
            [ input [ placeholder "City, State", onInput OnInput ] []
            , button [ id "submit", onClick OnClick ] [ text "Enter" ]
            ]
        , div [ id "weather-panel" ]
            [ text << .description << .condition <| mod
            , br [] []
            , viewTemp mod
            ]
        , div [ id "error-panel" ]
            [ text << .error <| mod ]
        , div [ id "time-panel" ]
            [ text << toString << .time  <| mod ]
        ]


stringToMaybeIntDecoder : Decoder (Maybe Int)
stringToMaybeIntDecoder =
    Decode.map (Result.toMaybe << String.toInt) Decode.string


decodeCondition : Decoder Condition
decodeCondition =
    map2
        Condition
        (at [ "query", "results", "channel", "item", "condition", "text" ] Decode.string)
        (at [ "query", "results", "channel", "item", "condition", "temp" ] stringToMaybeIntDecoder)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mod =
    case msg of
        OnInput s ->
            ( { mod | cityState = s }, Cmd.none )

        OnClick ->
            ( mod
            , Http.send NewRequest
                (Http.get
                    (String.concat [ baseUrl, constructURL mod.cityState, suffixURL ])
                    decodeCondition
                )
            )

        NewRequest (Ok cond) ->
            ( { mod | condition = cond }, Cmd.none )

        NewRequest (Err e) ->
            ( { mod | error = toString e }, Cmd.none )

        Tick t ->
            ( { mod | time = t }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions mod =
    Time.every Time.second Tick
