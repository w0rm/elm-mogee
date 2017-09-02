module Model
    exposing
        ( Model
        , initial
        , update
        , GameState(..)
        )

import Components.Keys as Keys exposing (Keys, codes)
import Time exposing (Time)
import WebGL.Texture exposing (Texture, Error)
import Messages exposing (Msg(..))
import Systems.Systems as Systems exposing (Systems)
import Components.Components as Components exposing (Components)
import Components.Menu as Menu exposing (Menu)
import PageVisibility exposing (Visibility(..))
import Dict


type GameState
    = Paused
    | Playing
    | Dead
    | Initial Menu


type alias Model =
    { systems : Systems
    , components : Components
    , state : GameState
    , lives : Int
    , score : Int
    , size : Int
    , sound : Bool
    , texture : Maybe Texture
    , sprite : Maybe Texture
    , font : Maybe Texture
    , keys : Keys
    }


initial : Model
initial =
    { components = Components.initial
    , systems = Systems.initial
    , lives = 0
    , score = 0
    , state = Initial Menu.initial
    , size = 0
    , sound = True
    , texture = Nothing
    , font = Nothing
    , sprite = Nothing
    , keys = Keys.initial
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Resize { width, height } ->
            { model | size = min width height // 64 * 64 } ! []

        Animate elapsed ->
            animateKeys elapsed (animate (min elapsed 60 * 1.5) model) ! []

        KeyChange pressed keyCode ->
            { model | keys = Keys.keyChange pressed keyCode model.keys } ! []

        TextureLoaded texture ->
            { model | texture = Result.toMaybe texture } ! []

        SpriteLoaded sprite ->
            { model | sprite = Result.toMaybe sprite } ! []

        FontLoaded font ->
            { model | font = Result.toMaybe font } ! []

        VisibilityChange Visible ->
            model ! []

        VisibilityChange Hidden ->
            { model
                | state =
                    if model.state == Playing then
                        Paused
                    else
                        model.state
            }
                ! []


animate : Time -> Model -> Model
animate elapsed model =
    case model.state of
        Paused ->
            if Keys.pressed codes.enter model.keys then
                { model | state = Playing }
            else
                model

        Dead ->
            if Keys.pressed codes.enter model.keys then
                if model.lives == 0 then
                    { model | state = Initial Menu.initial }
                else
                    continue model
            else
                model

        Initial menu ->
            let
                ( newMenu, cmd ) =
                    Menu.update model.sound model.keys menu
            in
                case cmd of
                    Menu.Start ->
                        start { model | state = Initial newMenu }

                    Menu.ToggleSound sound ->
                        { model | sound = sound, state = Initial newMenu }

                    Menu.Noop ->
                        { model | state = Initial newMenu }

        Playing ->
            let
                ( newComponents, newSystems ) =
                    Systems.run elapsed (Keys.directions model.keys) model.components model.systems

                state =
                    if Keys.pressed codes.escape model.keys then
                        Paused
                    else
                        model.state
            in
                checkLives
                    { model
                        | components = newComponents
                        , systems = newSystems
                        , state = state
                    }


animateKeys : Time -> Model -> Model
animateKeys elapsed model =
    { model | keys = Keys.animate elapsed model.keys }


checkLives : Model -> Model
checkLives model =
    if Components.isDead model.components then
        { model
            | lives = model.lives - 1
            , state = Dead
        }
    else
        model


continue : Model -> Model
continue model =
    { model
        | state = Playing
        , components = Components.initial
        , systems = Systems.initial
        , score = model.score + model.systems.currentScore
    }


start : Model -> Model
start model =
    { model
        | state = Playing
        , components = Components.initial
        , systems = Systems.initial
        , lives = 3
        , score = 0
    }
