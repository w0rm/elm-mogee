module Mogee exposing (main)

import WebGL.Texture as Texture exposing (defaultOptions)
import Keyboard
import Model exposing (Model)
import Task exposing (Task)
import Messages exposing (Msg(..))
import View
import Window
import AnimationFrame
import Html
import View.Font as Font
import View.Sprite as Sprite


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        , Window.resizes Resize
        ]


init : ( Model, Cmd Msg )
init =
    ( Model.model
    , Cmd.batch
        [ Sprite.loadTexture TextureLoaded
        , Font.load FontLoaded
        , Task.perform Resize Window.size
        ]
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = View.view
        , subscriptions = subscriptions
        , update = Model.update
        }
