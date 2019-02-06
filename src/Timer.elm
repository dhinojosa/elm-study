module Timer exposing (main)

import Browser exposing (element)
import Html exposing (Html, text)
import String exposing (fromInt)
import Task exposing (perform)
import Time exposing (..)
import Debug exposing (log)


type alias Model =
    { zone : Maybe Zone, display : String, error : String }


type Action
    = GetError
    | GetZone Zone
    | GetPosix Posix


main : Program () Model Action
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Action )
init fg =
    ( { zone = Nothing, display = "", error = "" }, perform GetZone here )


view : Model -> Html Action
view mod =
    text mod.display

toEnglishMonth : Month -> String
toEnglishMonth month =
  case month of
    Jan -> "January"
    Feb -> "February"
    Mar -> "March"
    Apr -> "April"
    May -> "May"
    Jun -> "June"
    Jul -> "July"
    Aug -> "August"
    Sep -> "September"
    Oct -> "October"
    Nov -> "November"
    Dec -> "December"

update : Action -> Model -> ( Model, Cmd Action )
update act mod =
    case act of
        GetError ->
            ( { mod | error = "oops" }, Cmd.none )

        GetZone z ->
            ( { mod | zone = Just z }, Cmd.none )

        GetPosix p ->
            case mod.zone of
                Just z ->
                    let
                        month =
                            toMonth z p

                        day =
                            toDay z p

                        year =
                            toYear z p

                        hour =
                            toHour z p

                        minute =
                            toMinute z p

                        second =
                            toSecond z p
                    in
                    ( { mod | display = 
                         toEnglishMonth month ++ " " ++
                         fromInt day ++ " " ++
                         fromInt year ++ " " ++
                         fromInt hour ++ ":" ++
                         fromInt minute ++":" ++ 
                         fromInt second }, Cmd.none )

                Nothing ->
                    ( { mod | display = "Zone not found" }, Cmd.none )


subscriptions : Model -> Sub Action
subscriptions mod =
    every 1000 GetPosix
