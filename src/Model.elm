module Model (Model, Keys, model, update, mogee, GameState(..)) where

import Model.Object as Object exposing (Object)
import Model.Direction as Direction exposing (Direction(..))
import Time exposing (Time)
import Random


type GameState = Paused | Playing | Stopped


type alias Model =
  { objects : List Object
  , direction : Direction
  , seed : Random.Seed
  , state : GameState
  , lives : Int
  , score : Int
  , currentScore : Int
  , screens : Int
  }


type alias Keys = {x : Int, y : Int}


model : Model
model =
  { objects = Object.mogee (28, 27) :: Object.walls Left Right 0
  , direction = Right
  , seed = Random.initialSeed 1
  , lives = 3
  , screens = 0
  , score = 0
  , currentScore = 0
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
        , score = m.score
        }
      else
        m
    Stopped ->
      if enter then
        { m
        | state = Playing
        , score = 0
        }
      else
        m
    Playing ->
      m
      |> updateObjects elapsed keys
      |> addScreen
      |> checkLives


mogee : Model -> Object
mogee {objects} =
  objects
    |> List.filter Object.isMogee
    |> List.head
    |> Maybe.withDefault (Object.mogee (28, 27))


updateObjects : Time -> Keys -> Model -> Model
updateObjects elapsed keys model =
  let
    objects = Object.cleanup model.objects
    screens = List.filter Object.isScreen objects
    number = screens
      |> List.filter (Object.collide (mogee model))
      |> List.map .number
      |> List.maximum
      |> Maybe.withDefault 0
    walls = List.filter Object.isWall objects
    updateObject = Object.update elapsed keys screens walls
  in
    { model
    | objects = List.foldr updateObject [] objects
    , currentScore = max number model.currentScore
    }


addScreen : Model -> Model
addScreen model =
  let
    (x, y) = model |> mogee |> .position
    (screenX, screenY) = (x - 32 + 4, y - 32 + 5)
    (direction, seed) = Random.generate (Direction.next model.direction) model.seed
  in
    if abs screenX < 64 && abs screenY < 64 then
      { model
      | seed = seed
      , direction = direction
      , screens = model.screens + 1
      , objects =
          ( Object.walls model.direction direction (model.screens + 1) ++
            List.map (Object.offset (Direction.opposite model.direction)) model.objects
          )
      }
    else
      model


checkLives : Model -> Model
checkLives m =
  if Object.isDead m.objects then
    if m.lives == 1 then
      { model
      | seed = m.seed
      , score = m.score + m.currentScore
      , currentScore = 0
      }
    else
      { m
      | lives = m.lives - 1
      , state = Paused
      , score = m.score + m.currentScore
      , currentScore = 0
      }
  else
    m
