module Model
    exposing
        ( Model
        , model
        , update
        , GameState(..)
        )

import Components.Keys as Keys exposing (Keys)
import Time exposing (Time)
import WebGL.Texture exposing (Texture, Error)
import Messages exposing (Msg(..))
import Systems.Systems as Systems exposing (Systems)
import Components.Components as Components exposing (Components)


type GameState
    = Paused
    | Playing
    | Stopped


type alias Model =
    { systems : Systems
    , components : Components
    , state : GameState
    , lives : Int
    , score : Int
    , size : Int
    , texture : Maybe Texture
    , font : Maybe Texture
    , keys : Keys
    }


model : Model
model =
    { components = Components.components
    , systems = Systems.systems
    , lives = 3
    , score = 0
    , state = Stopped
    , size = 0
    , texture = Nothing
    , font = Nothing
    , keys = Keys False False False False False False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Resize { width, height } ->
            { model | size = min width height // 64 * 64 } ! []

        Animate elapsed ->
            animate (min elapsed 60 * 1.5) model ! []

        KeyChange pressed keyCode ->
            { model | keys = Keys.keyChange pressed keyCode model.keys } ! []

        TextureLoaded texture ->
            { model | texture = Result.toMaybe texture } ! []

        FontLoaded font ->
            { model | font = Result.toMaybe font } ! []


animate : Time -> Model -> Model
animate elapsed m =
    case m.state of
        Paused ->
            if m.keys.enter then
                { model
                    | state = Playing
                    , lives = m.lives
                    , texture = m.texture
                    , font = m.font
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
            let
                ( newComponents, newSystems ) =
                    Systems.run elapsed (Keys.directions m.keys) m.components m.systems
            in
                checkLives
                    { m
                        | components = newComponents
                        , systems = newSystems
                    }


checkLives : Model -> Model
checkLives m =
    if Components.isDead m.components then
        if m.lives == 1 then
            { model
                | score = m.score + m.systems.currentScore
                , texture = m.texture
                , font = m.font
                , size = m.size
                , systems = Systems.systems
            }
        else
            { m
                | lives = m.lives - 1
                , state = Paused
                , score = m.score + m.systems.currentScore
                , systems = Systems.systems
            }
    else
        m
