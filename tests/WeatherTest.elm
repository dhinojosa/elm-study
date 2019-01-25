module WeatherTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Weather exposing (constructURL)
import Json.Decode exposing (..)
import Result exposing (Result (..))


sampleResult : String
sampleResult = """ {\"query\":{\"count\":1,\"created\":\"2017-12-04T17:50:55Z\",\"lang\":\"en-US\",\"results\":{\"channel\":{\"units\":{\"distance\":\"mi\",\"pressure\":\"in\",\"speed\":\"mph\",\"temperature\":\"F\"},\"title\":\"Yahoo! Weather - Clearwater, FL, US\",\"link\":\"http://us.rd.yahoo.com/dailynews/rss/weather/Country__Country/*https://weather.yahoo.com/country/state/city-2381303/\",\"description\":\"Yahoo! Weather for Clearwater, FL, US\",\"language\":\"en-us\",\"lastBuildDate\":\"Mon, 04 Dec 2017 12:50 PM EST\",\"ttl\":\"60\",\"location\":{\"city\":\"Clearwater\",\"country\":\"United States\",\"region\":\" FL\"},\"wind\":{\"chill\":\"75\",\"direction\":\"95\",\"speed\":\"14\"},\"atmosphere\":{\"humidity\":\"66\",\"pressure\":\"1021.0\",\"rising\":\"0\",\"visibility\":\"16.1\"},\"astronomy\":{\"sunrise\":\"7:7 am\",\"sunset\":\"5:35 pm\"},\"image\":{\"title\":\"Yahoo! Weather\",\"width\":\"142\",\"height\":\"18\",\"link\":\"http://weather.yahoo.com\",\"url\":\"http://l.yimg.com/a/i/brand/purplelogo//uh/us/news-wea.gif\"},\"item\":{\"title\":\"Conditions for Clearwater, FL, US at 12:00 PM EST\",\"lat\":\"27.978239\",\"long\":\"-82.75676\",\"link\":\"http://us.rd.yahoo.com/dailynews/rss/weather/Country__Country/*https://weather.yahoo.com/country/state/city-2381303/\",\"pubDate\":\"Mon, 04 Dec 2017 12:00 PM EST\",\"condition\":{\"code\":\"28\",\"date\":\"Mon, 04 Dec 2017 12:00 PM EST\",\"temp\":\"74\",\"text\":\"Mostly Cloudy\"},\"forecast\":[{\"code\":\"30\",\"date\":\"04 Dec 2017\",\"day\":\"Mon\",\"high\":\"79\",\"low\":\"64\",\"text\":\"Partly Cloudy\"},{\"code\":\"32\",\"date\":\"05 Dec 2017\",\"day\":\"Tue\",\"high\":\"81\",\"low\":\"65\",\"text\":\"Sunny\"},{\"code\":\"34\",\"date\":\"06 Dec 2017\",\"day\":\"Wed\",\"high\":\"81\",\"low\":\"64\",\"text\":\"Mostly Sunny\"},{\"code\":\"28\",\"date\":\"07 Dec 2017\",\"day\":\"Thu\",\"high\":\"73\",\"low\":\"61\",\"text\":\"Mostly Cloudy\"},{\"code\":\"47\",\"date\":\"08 Dec 2017\",\"day\":\"Fri\",\"high\":\"65\",\"low\":\"57\",\"text\":\"Scattered Thunderstorms\"},{\"code\":\"39\",\"date\":\"09 Dec 2017\",\"day\":\"Sat\",\"high\":\"63\",\"low\":\"54\",\"text\":\"Scattered Showers\"},{\"code\":\"34\",\"date\":\"10 Dec 2017\",\"day\":\"Sun\",\"high\":\"61\",\"low\":\"53\",\"text\":\"Mostly Sunny\"},{\"code\":\"32\",\"date\":\"11 Dec 2017\",\"day\":\"Mon\",\"high\":\"67\",\"low\":\"51\",\"text\":\"Sunny\"},{\"code\":\"34\",\"date\":\"12 Dec 2017\",\"day\":\"Tue\",\"high\":\"72\",\"low\":\"53\",\"text\":\"Mostly Sunny\"},{\"code\":\"30\",\"date\":\"13 Dec 2017\",\"day\":\"Wed\",\"high\":\"69\",\"low\":\"57\",\"text\":\"Partly Cloudy\"}],\"description\":\"<![CDATA[<img src=\\\"http://l.yimg.com/a/i/us/we/52/28.gif\\\"/>\\n<BR />\\n<b>Current Conditions:</b>\\n<BR />Mostly Cloudy\\n<BR />\\n<BR />\\n<b>Forecast:</b>\\n<BR /> Mon - Partly Cloudy. High: 79Low: 64\\n<BR /> Tue - Sunny. High: 81Low: 65\\n<BR /> Wed - Mostly Sunny. High: 81Low: 64\\n<BR /> Thu - Mostly Cloudy. High: 73Low: 61\\n<BR /> Fri - Scattered Thunderstorms. High: 65Low: 57\\n<BR />\\n<BR />\\n<a href=\\\"http://us.rd.yahoo.com/dailynews/rss/weather/Country__Country/*https://weather.yahoo.com/country/state/city-2381303/\\\">Full Forecast at Yahoo! Weather</a>\\n<BR />\\n<BR />\\n<BR />\\n]]>\",\"guid\":{\"isPermaLink\":\"false\"}}}}}}"""

suite : Test
suite =
    describe "constructing an url from base query"
        [ test "the base query should include the city, Austin, TX" <|
            \_ ->
                constructURL "Austin, TX"
                    |> Expect.equal
                        "select * from weather.forecast where woeid in (select woeid from geo.places(1) where text=\"Austin, TX\")"
          , test "Parsing the content from yahoo test" <|
             \_ ->
                decodeString (at ["query", "results", "channel", "item", "condition", "text"] string) sampleResult
                      |> Expect.equal (Ok "Mostly Cloudy")
        ]
