module Model (Model, Keys, model, update, offset, GameState(..)) where

import Model.Object as Object exposing (Object)
import Model.Screen as Screen exposing (Screen)
import Model.Direction exposing (Direction(..))
import Time exposing (Time)
import Random


type GameState = Paused | Playing | Stopped


type alias Model =
  { objects : List Object
  , screens : List Screen
  , seed : Random.Seed
  , state : GameState
  , lives : Int
  }


type alias Keys = {x : Int, y : Int}


model : Model
model =
  let
    screen = Screen.screen (0, 0) 0 Right
  in
    { objects = Screen.walls Left screen ++ [Object.mogee (28, 27)]
    , screens = [screen]
    , seed = Random.initialSeed 1
    , lives = 3
    , state = Stopped
    }


update : (Time, Keys, Bool) -> Model -> Model
update (elapsed, keys, enter) m =
  case m.state of
    Paused ->
      if enter then
        { model
        | state = Playing
        , lives = m.lives
        , seed = m.seed
        }
      else
        m
    Stopped ->
      if enter then
        { m | state = Playing }
      else
        m
    Playing ->
      { m
      | objects = List.foldr (Object.update (elapsed, keys) m.objects) [] m.objects
      }
      |> addScreen
      |> removeScreens
      |> checkLives


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

    lastScr = Maybe.withDefault (Screen.screen (0, 0) 0 Right) (List.head model.screens)
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


removeScreens : Model -> Model
removeScreens model =
  let
    numbers = model.objects
      |> List.filter (\o -> Object.isSpace o && (fst o.size == 0 || snd o.size == 0))
      |> List.map .number
  in
    { model
    | screens =
        List.filter
          (\{number} -> List.all ((/=) number) numbers)
          model.screens
    }


checkLives : Model -> Model
checkLives m =
  if Object.isDead m.objects then
    if m.lives == 1 then
      { model
      | seed = m.seed
      }
    else
      { m
      | lives = m.lives - 1
      , state = Paused
      }
  else
    m
