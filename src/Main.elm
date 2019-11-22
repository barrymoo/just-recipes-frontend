module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Url
import Array

-- MAIN

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


-- MODEL


type alias Model =
  { ingredients : Array.Array (Int, String)
  , count : Int
  , key : Nav.Key
  , url : Url.Url
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  (Model (Array.fromList [(0, "")]) 1 key url, Cmd.none)

-- UPDATE

type Msg
  = Add
  | Set Int String
  | Remove Int
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Add ->
      ( { model
        | ingredients = Array.append
          model.ingredients (Array.fromList [(model.count, "")])
        , count = model.count + 1
        }
      , Cmd.none
      )
    Set idx v ->
      ( { model
          | ingredients =  model.ingredients
                      |> Array.set idx (idx, v)
        }
      , Cmd.none
      )
    Remove idx ->
      let firstNotEqIdx = Tuple.first >> (==) idx >> not
          ingredientStrs = Array.filter firstNotEqIdx model.ingredients
            |> Array.map Tuple.second
      in
          ( { model
              | count = model.count - 1
              , ingredients = Array.toIndexedList ingredientStrs |> Array.fromList
            }
          , Cmd.none
          )
    LinkClicked urlRequest ->
      case urlRequest of
         Browser.Internal url ->
           ( model, Nav.pushUrl model.key (Url.toString url) )

         Browser.External href ->
           ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Just Recipes"
  , body =
    [
      div [] [
        div []
          ( Array.map (\(idx, v) -> viewInput "text" "Ingredient" idx v)
            model.ingredients
            |> Array.toList
          )
        , div [] [ button [ onClick Add ] [ text "+" ] ]
        , div [] [ viewValidation model ]
        ]
    ]
  }

viewInput : String -> String -> Int -> String -> Html Msg
viewInput t p idx v =
  div []
    [ input [ type_ t, placeholder p, value v, onInput (Set idx) ] []
    , button [ onClick (Remove idx) ] [ text "-" ]
    ]

viewValidation : Model -> Html msg
viewValidation model =
  if model.count == 0 then
    div [ style "color" "red" ] [ text "Recipe has no ingredients?" ]
  else if Array.toList model.ingredients
     |> List.all (Tuple.second >> String.isEmpty >> not) then
    div [ style "color" "green" ] [ text "Nice recipe!"]
  else
    div [ style "color" "red" ] [ text "Empty ingredients box?" ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
