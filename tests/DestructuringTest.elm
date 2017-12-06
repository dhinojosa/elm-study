module DestructuringTest exposing (suite)

import Expect exposing (..)
import Test exposing (..)

myfunction : (Int, String) -> String
myfunction (i, s) = s ++ (toString i)

suite : Test
suite =
    describe "Destructuring"
        [ test "Pattern Matching a List" <|
            \_ ->
               let 
                   a = [1,2,3,4]
                   r = case a of
                      [] -> "Empty"
                      a :: [] -> "Single: " ++ (toString a)
                      a :: b :: [] -> "Dual: " ++ (toString <| a + b)
                      a :: b :: _ -> "Has at least two: " ++ (toString <| a + b)
               in 
                  Expect.equal r "Has at least two: 3"
          , test "Destructuring a Tuple" <|
            \_ ->
               let
                   t = ("A", 4.0, (3, 'a'))
                   (s, d, (i , c)) = t
                   r = s ++ (toString c) ++ (toString d) ++ (toString i)
               in
                  Expect.equal r "A'a'43"
          , test "Destructuring a Tuple in a function" <|
            \_ ->
               let
                   r = myfunction (3, "Foo")
               in
                   Expect.equal r "Foo3"
          , test "Pattern Matching a Tuple" <|
            \_ ->
               let
                   t = ("A", 3, 4.0)
                   r = case t of
                      (s, i, d) -> s ++ (toString <| i + d)
               in
                  Expect.equal r "A7"
          , test "Pattern Matching a Record" <|
            \_ -> 
               let 
                   myRecord = {x = 3, y = 10}
                   {x, y} = myRecord
                   r = x * y
               in
                  Expect.equal r 30
          , test "Destructuring a Record" <|
             \_ ->
               let 
                   joffrey = { name = "Joffrey Baratheon",
                               houses = ["House Baratheon", 
                                         "House Lannister"],
                               location = "King's Landing",
                               age = 19 }
                   {name, age} = joffrey
                   r = name ++ " is " ++ (toString age)
               in 
                  Expect.equal r "Joffrey Baratheon is 19"
        ]
