module Model (Model, Keys, model, update) where

import Model.Object as Object exposing (Object)
import Model.Screen as Screen exposing (Screen, Direction(..))
import Time exposing (Time)
import Random


type alias Model =
  { objects : List Object
  , screens : List Screen
  , seed : Random.Seed
  }


type alias Keys = {x : Int, y : Int}


model : Model
model =
  let
    screen = Screen.screen (0, 0) Right
  in
    { objects = Object.mogee (5, 5) :: Screen.walls Left screen
    , screens = [screen]
    , seed = Random.initialSeed 0
    }
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen
    |> addScreen


update : (Time, Keys) -> Model -> Model
update (elapsed, keys) model =
  { model
  | objects = List.map (Object.update (elapsed, keys) model.objects) model.objects
  }


addScreen : Model -> Model
addScreen model =
  let
    lastScr = Maybe.withDefault (Screen.screen (0, 0) Right) (List.head model.screens)
    (scr, seed) = Random.generate (Screen.next model.screens) model.seed
  in
    { model
    | seed = seed
    , screens = scr :: model.screens
    , objects = Screen.walls lastScr.direction scr ++ model.objects
    }
