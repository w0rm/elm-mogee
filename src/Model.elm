module Model (Model, Keys, model, update) where

import Object exposing (Object)
import Screen exposing (Screen)
import Direction exposing (Direction(..))
import Time exposing (Time)


type alias Model =
  { objects : List Object
  , screens : List Screen
  }


type alias Keys = {x : Int, y : Int}


model : Model
model =
  let
    screen = Screen.screen (0, 0) Right
  in
    { objects =
        Object.mogee (5, 5) ::
        Object.wall (20, 2) (10, 30) ::
        Object.wall (20, 2) (40, 40) ::
        Object.wall (20, 2) (20, 50) ::
        Object.wall (2, 60) (62, 2) ::
        Screen.walls Right screen
    , screens = [screen]
    }


update : (Time, Keys) -> Model -> Model
update (elapsed, keys) model =
  { model
  | objects = List.map (Object.update (elapsed, keys) model.objects) model.objects
  }
