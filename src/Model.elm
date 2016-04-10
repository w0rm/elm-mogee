module Model (Model, Keys, model, update) where

import Object exposing (Object)
import Screen exposing (Screen)
import Direction exposing (Direction(..))
import Math.Vector2 exposing (Vec2, vec2)
import Time exposing (Time)


type alias Model =
  { objects : List Object
  , screens : List Screen
  }


type alias Keys = {x : Int, y : Int}


model : Model
model =
  let
    screen = Screen.screen (vec2 0 0) Right
  in
    { objects =
        Object.mogee (vec2 5 5) ::
        Object.wall (vec2 20 2) (vec2 10 30) ::
        Object.wall (vec2 20 2) (vec2 40 40) ::
        Object.wall (vec2 20 2) (vec2 20 50) ::
        Object.wall (vec2 2 60) (vec2 62 2) ::
        Screen.walls Right screen
    , screens = [screen]
    }


update : (Time, Keys) -> Model -> Model
update (elapsed, keys) model =
  { model
  | objects = List.map (Object.update (elapsed, keys) model.objects) model.objects
  }
