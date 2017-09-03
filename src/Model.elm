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
    , time : Float
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
    , time = 0
    , texture = Nothing
    , font = Nothing
    , sprite = Nothing
    , keys = Keys.initial
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Resize { width, height } ->
            ( { model | size = min width height // 64 * 64 }
            , Cmd.none
            )

        Animate elapsed ->
            ( model
                |> animate elapsed
                |> animateKeys elapsed
            , Cmd.none
            )

        KeyChange pressed keyCode ->
            ( { model | keys = Keys.keyChange pressed keyCode model.keys }
            , Cmd.none
            )

        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }
            , Cmd.none
            )

        SpriteLoaded sprite ->
            ( { model | sprite = Result.toMaybe sprite }
            , Cmd.none
            )

        FontLoaded font ->
            ( { model | font = Result.toMaybe font }
            , Cmd.none
            )

        VisibilityChange Visible ->
            ( model, Cmd.none )

        VisibilityChange Hidden ->
            ( { model
                | state =
                    if model.state == Playing then
                        Paused
                    else
                        model.state
              }
            , Cmd.none
            )


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
                    Menu.update elapsed model.sound model.keys menu
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
                limitElapsed =
                    min elapsed 60

                ( newComponents, newSystems ) =
                    Systems.run limitElapsed (Keys.directions model.keys) model.components model.systems

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
