import Html exposing (Html, text, ol, li)
import List exposing (map, sort)

main : Html msg
main = let groceries = ["Apples", "Naan", "Eggs", "Milk", "Seeds"]
           sortedGroceries = sort groceries in
               ol [] (map (\x -> li[][text x]) sortedGroceries)
