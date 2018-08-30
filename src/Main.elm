module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events
    exposing
        ( onAnimationFrameDelta
        , onKeyDown
        , onKeyUp
        , onResize
        , onVisibilityChange
        )
import Html.Events exposing (keyCode)
import Json.Decode as Decode exposing (Value)
import Messages exposing (Msg(..))
import Model exposing (Model)
import Task exposing (Task)
import View
import View.Font as Font
import View.Sprite as Sprite


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Animate
        , onKeyDown (Decode.map (KeyChange True) keyCode)
        , onKeyUp (Decode.map (KeyChange False) keyCode)
        , onResize Resize
        , onVisibilityChange VisibilityChange
        ]


init : Value -> ( Model, Cmd Msg )
init _ =
    ( Model.initial
    , Cmd.batch
        [ Sprite.loadTexture TextureLoaded
        , Sprite.loadSprite SpriteLoaded
        , Font.load FontLoaded
        , Task.perform (\{ viewport } -> Resize (round viewport.width) (round viewport.height)) getViewport
        ]
    )


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = View.view
        , subscriptions = subscriptions
        , update = Model.update
        }
