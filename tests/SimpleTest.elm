module SimpleTest exposing (..)

import Test exposing (Test, describe, test)
import Expect

all:Test
all = describe "Sample Test Suite" [
         test "Addition" (\_ ->
            Expect.equal 4 4
         ),
         test "Multiply" (\_ ->
            Expect.equal (4 * 10) 40
         )
      ]
