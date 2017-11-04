import Html exposing (Html, text, div, input)
import Html.Events exposing (onInput)
import String exposing (reverse, uncons, cons, toLower, append)
import List exposing (member)
import Char

type alias Model = String

model : Model
model = ""

type OnInputEvent = OnInputEvent String

main : Program Never Model OnInputEvent
main = Html.beginnerProgram {
   model = model,
   view = view,
   update = update}

view : Model -> Html OnInputEvent
view mod = div[] [
      text "Enter word:",
      input[onInput OnInputEvent][],
      text mod
   ]

appendToString : String -> Char -> String
appendToString s c = (reverse (cons c (reverse s)))

isLowerCaseVowel : Char -> Bool
isLowerCaseVowel c = member c ['a', 'e', 'i', 'o', 'u']

isLowerCaseConsonant : Char -> Bool
isLowerCaseConsonant c = not << isLowerCaseVowel <| c

capFirst : String -> String
capFirst s = case (uncons s) of 
                Just (h, t) -> cons (Char.toUpper h) t
                Nothing     -> ""

consPigLatin : String -> String -> String
consPigLatin acc word = case uncons word of
                           Just(h, t) -> 
                               if (isLowerCaseConsonant h) then
                                  consPigLatin (appendToString acc h) t
                               else
                                  word ++ acc ++ "ay"
                           Nothing -> ""

pigLatin : String -> String
pigLatin s = case (uncons << toLower <| s) of
                Just (h, t) ->
                      if (isLowerCaseVowel h) then (cons h t) ++ "way"
                      else consPigLatin "" s
                Nothing -> ""

update : OnInputEvent -> Model -> Model
update evt mod = case evt of
                    OnInputEvent s -> capFirst << pigLatin <| s
