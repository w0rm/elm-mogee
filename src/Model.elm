module Model
    exposing
        ( Model
        , model
        , update
        , mogee
        , GameState(..)
        )

import Model.Object as Object exposing (Object)
import Model.Direction as Direction exposing (Direction(..))
import Model.Keys as Keys exposing (Keys)
import Time exposing (Time)
import Random
import WebGL exposing (Texture)
import Actions exposing (Action(..))


type GameState
    = Paused
    | Playing
    | Stopped


type alias Model =
    { objects : List Object
    , direction : Direction
    , seed : Random.Seed
    , state : GameState
    , lives : Int
    , score : Int
    , currentScore : Int
    , screens : Int
    , size : Int
    , texture : Maybe Texture
    , keys : Keys
    }


model : Model
model =
    { objects = Object.mogee ( 28, 27 ) :: Object.walls Left Right 0
    , direction = Right
    , seed = Random.initialSeed 1
    , lives = 3
    , screens = 0
    , score = 0
    , currentScore = 0
    , state = Stopped
    , size = 0
    , texture = Nothing
    , keys = Keys False False False False False False
    }


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Resize { width, height } ->
            { model | size = min width height // 64 * 64 } ! []

        Animate elapsed ->
            animate (min elapsed 25 * 1.5) model ! []

        KeyChange func ->
            { model | keys = func model.keys } ! []

        TextureLoaded texture ->
            { model | texture = Just texture } ! []

        _ ->
            model ! []


animate : Time -> Model -> Model
animate elapsed m =
    case m.state of
        Paused ->
            if m.keys.enter then
                { model
                    | state = Playing
                    , lives = m.lives
                    , seed = m.seed
                    , texture = m.texture
                    , size = m.size
                    , score = m.score
                }
            else
                m

        Stopped ->
            if m.keys.enter then
                { m
                    | state = Playing
                    , score = 0
                }
            else
                m

        Playing ->
            m
                |> updateObjects elapsed (Keys.directions m.keys)
                |> addScreen
                |> checkLives


mogee : Model -> Object
mogee { objects } =
    objects
        |> List.filter Object.isMogee
        |> List.head
        |> Maybe.withDefault (Object.mogee ( 28, 27 ))


updateObjects : Time -> { x : Int, y : Int } -> Model -> Model
updateObjects elapsed keys model =
    let
        objects =
            Object.cleanup model.objects

        screens =
            List.filter Object.isScreen objects

        number =
            screens
                |> List.filter (Object.collide (mogee model))
                |> List.map .number
                |> List.maximum
                |> Maybe.withDefault 0

        walls =
            List.filter Object.isWall objects

        updateObject =
            Object.update elapsed keys screens walls
    in
        { model
            | objects = List.foldr updateObject [] objects
            , currentScore = max number model.currentScore
        }


addScreen : Model -> Model
addScreen model =
    let
        ( x, y ) =
            model |> mogee |> .position

        ( screenX, screenY ) =
            ( x - 32 + 4, y - 32 + 5 )

        ( direction, seed ) =
            Random.step (Direction.next model.direction) model.seed
    in
        if abs screenX < 64 && abs screenY < 64 then
            { model
                | seed = seed
                , direction = direction
                , screens = model.screens + 1
                , objects =
                    (Object.walls model.direction direction (model.screens + 1)
                        ++ List.map (Object.offset (Direction.opposite model.direction)) model.objects
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
                , texture = m.texture
                , size = m.size
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
