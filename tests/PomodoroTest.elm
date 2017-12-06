module PomodoroTest exposing (all)

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

import Dict
import Expect
import Html
import Html.Attributes exposing (class)
import Maybe
import Pomodoro exposing (updateEstimate, viewErrors)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (id, tag, text)
import Tuple


all : Test
all =
    describe "Pomodoro"
        [ describe "viewErrors"
            [ test "Given errors two errors on a list should product a list"
                (\_ ->
                    let
                        list =
                            [ ( "my-id", [ "Error One", "Error Two" ] ) ]

                        dict =
                            Dict.fromList list

                        result =
                            viewErrors "my-id" dict
                    in
                    result
                        |> Query.fromHtml
                        |> Query.findAll [ tag "li" ]
                        |> Query.count (Expect.equal 2)
                )
            ]
        , describe "updateEstimate"
            [ test "Given a string with all integers, the model errors should be empty"
                (\_ ->
                    let
                        input =
                            "400"

                        mod =
                            { list = []
                            , latest = Pomodoro.PomodoroLineItem "Foo" 3 0 0
                            , errors = Dict.empty
                            }

                        result =
                            updateEstimate "my-input" input mod
                                |> Tuple.first
                                >> .errors
                                >> Dict.isEmpty
                    in
                    Expect.true "Expecting the model errors to be empty" result
                )
            , test "Given a string with that cannot be converted, an error should be added"
                (\_ ->
                    let
                        input =
                            "300d"

                        mod =
                            { list = []
                            , latest = Pomodoro.PomodoroLineItem "Foo" 3 0 0
                            , errors = Dict.empty
                            }

                        dict =
                            updateEstimate "estimate-input" input mod
                                |> Tuple.first
                                >> .errors

                        entry =
                            Dict.get "estimate-input" dict
                    in
                    case entry of
                        Just msgs ->
                            Expect.equal (List.length msgs) 1

                        Nothing ->
                            Expect.fail "Expecting key estimate-input"
                )
            ]
        ]
