module PalindromeTest exposing (all)

import Palindrome exposing (palindrome)
import Test exposing (Test, describe, test)
import Expect

all:Test
all = describe "Palindrome" [
         test "An empty String should return an empty String" (\_ ->
            Expect.equal (palindrome "") ""
         )
      ]
