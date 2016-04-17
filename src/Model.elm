module Model (Model, Keys, model, update, offset) where

import Model.Object as Object exposing (Object)
import Model.Screen as Screen exposing (Screen, Direction(..))
import Time exposing (Time)
import Random


type alias Model =
  { objects : List Object
  , screens : List Screen
  , seed : Random.Seed
  , lives : Int
  }


type alias Keys = {x : Int, y : Int}


model : Model
model =
  let
    screen = Screen.screen (0, 0) Right
  in
    { objects = Screen.walls Left screen ++ [Object.mogee (5, 5)]
    , screens = [screen]
    , seed = Random.initialSeed 1
    , lives = 3
    }


update : (Time, Keys) -> Model -> Model
update (elapsed, keys) model =
  { model
  | objects = List.map (Object.update (elapsed, keys) model.objects) model.objects
  }
  |> addScreen


offset : Model -> (Float, Float)
offset {objects} =
  objects
    |> List.filter Object.isMogee
    |> List.head
    |> Maybe.map .position
    |> Maybe.withDefault (0, 0)


addScreen : Model -> Model
addScreen model =
  let
    (x, y) = offset model

    pos = (round (x / 64), round (y / 64))

    lastScrOffsets = List.take 3 model.screens
      |> List.map (\{offset} -> (round (fst offset / 64), round (snd offset / 64)))

    lastScr = Maybe.withDefault (Screen.screen (0, 0) Right) (List.head model.screens)
    (scr, seed) = Random.generate (Screen.next model.screens) model.seed
  in
    if List.any ((==) pos) lastScrOffsets then
      { model
      | seed = seed
      , screens =
          (scr :: model.screens)
          |> List.map (Screen.offset scr.offset)
      , objects =
          (Screen.walls lastScr.direction scr ++ model.objects)
          |> List.map (Object.offset scr.offset)
      }
    else
      model
