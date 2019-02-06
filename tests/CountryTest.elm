module CountryTest exposing (sample, suite)

import Debug
import Expect exposing (..)
import Json.Decode exposing (..)
import Result exposing (Result(..), withDefault)
import Test exposing (..)
import String exposing (uncons)
import List exposing (foldl)


type alias Country =
    { name : String
    , nativeName : String
    , currencyName : String
    , flagPng : String
    }


type alias Result =
    List Country


suite : Test
suite =
    describe "Country Module"
        [ describe "JSON Parsing"
            [ test "Parse Costa Rica Capital" <|
                \_ ->
                    Expect.equal
                        (withDefault []
                            (decodeString
                                (field "Response" (list (field "Region" string)))
                                sample
                            )
                        )
                        [ "Americas" ]
            , test "Parse into objects" <|
                \_ ->
                    let
                        nameField =
                            field "Name" string

                        nativeNameField =
                            field "NativeName" string

                        currencyNameField =
                            field "CurrencyName" string

                        flagPngField =
                            field "FlagPng" string

                        countryDecoder =
                            map4 Country
                                nameField
                                nativeNameField
                                currencyNameField
                                flagPngField

                        defaultCountry =
                            Country "US" "USA" "Dollars" "zip"

                        result =
                            decodeString (field "Response" (list countryDecoder)) sample

                        expected =
                            { currencyName = "Costa Rican colón"
                            , flagPng = "https://api.backendless.com/2F26DFBF-433C-51CC-FF56-830CEA93BF00/473FB5A9-D20E-8D3E-FF01-E93D9D780A00/files/CountryFlagsPng/cri.png"
                            , name = "Costa Rica"
                            , nativeName = "Costa Rica"
                            }
                    in
                    Expect.equal
                        (withDefault [] result)
                        [ expected ]
            , test "Using a function to tidy things up" <| 
               \_ -> 
                  let 
                    result = decodeString countryList sample 
                    str  = case result of 
                       Err e -> errorToString e
                       Ok [] -> "No values"
                       Ok (a :: _) -> a.currencyName
                  in
                    Expect.equal str "Costa Rican colón"
            , test "mkString" <| 
              \_ ->
                 let 
                    xs = ["1","2","3","4"]
                    delim = "," 
                    result = String.join "," xs
                 in 
                    Expect.equal result "1,2,3,4"
            ]
        ]


countryList : Decoder ( List Country )
countryList =
    let
        nameField =
            field "Name" string

        nativeNameField =
            field "NativeName" string

        currencyNameField =
            field "CurrencyName" string

        flagPngField =
            field "FlagPng" string

        countryDecoder =
            map4 Country
                nameField
                nativeNameField
                currencyNameField
                flagPngField
    in

    field "Response" (list countryDecoder)


sample : String
sample =
    """
  {"IsSuccess":true,
   "UserMessage":null,
   "TechnicalMessage":null,
   "TotalCount":1,
   "Response":[
      {"Name":"Costa Rica",
       "Alpha2Code":"CR",
       "Alpha3Code":"CRI",
       "NativeName":"Costa Rica",
       "Region":"Americas",
       "SubRegion":"Central America",
       "Latitude":"10",
       "Longitude":"-84",
       "Area":51100,
       "NumericCode":188,
       "NativeLanguage":"spa",
       "CurrencyCode":"CRC",
       "CurrencyName":"Costa Rican colón",
       "CurrencySymbol":"₡",
       "Flag":"https://api.backendless.com/2F26DFBF-433C-51CC-FF56-830CEA93BF00/473FB5A9-D20E-8D3E-FF01-E93D9D780A00/files/CountryFlags/cri.svg",
       "FlagPng":"https://api.backendless.com/2F26DFBF-433C-51CC-FF56-830CEA93BF00/473FB5A9-D20E-8D3E-FF01-E93D9D780A00/files/CountryFlagsPng/cri.png"
      }
   ]
 }
"""
