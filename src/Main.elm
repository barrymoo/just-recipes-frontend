module Main exposing (..)

-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Array

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { ingredients : Array.Array (Int, String)
  , count : Int
  }

init : Model
init =
  Model (Array.fromList [(0, "")]) 1

-- UPDATE

type Msg
  = Add
  | Set Int String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Add ->
      { model
        | ingredients = Array.append
          model.ingredients (Array.fromList [(model.count, "")])
        , count = model.count + 1
      }
    Set idx v ->
      { model
        | ingredients =  model.ingredients
                      |> Array.set idx (idx, v)
      }

-- VIEW

view : Model -> Html Msg
view model =
  List.concat
  [ Array.map (\(idx, v) -> viewInput "text" "Ingredient" idx v)
      model.ingredients
      |> Array.toList
  , List.singleton ( button [ onClick Add ] [ text "+" ] )
  , List.singleton (viewValidation model)
  ] |>
  div []

viewInput : String -> String -> Int -> String -> Html Msg
viewInput t p idx v =
  input [ type_ t, placeholder p, value v, onInput (Set idx) ] []

viewValidation : Model -> Html msg
viewValidation model =
  if Array.map (\(_, v) -> String.isEmpty v |> not) model.ingredients
     |> Array.toList
     |> List.all identity
  then
    div [ style "color" "green" ] [ text "Nice recipe!"]
  else
    div [ style "color" "red" ] [ text "Empty ingredients?" ]
