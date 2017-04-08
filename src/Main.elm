module Mogee exposing (main)

import WebGL.Texture as Texture exposing (defaultOptions)
import Keyboard
import Model exposing (Model)
import Model.Keys as Keys
import Task exposing (Task)
import Messages exposing (Msg(..))
import View
import Window
import AnimationFrame
import Html


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs (Keys.keyChange True >> KeyChange)
        , Keyboard.ups (Keys.keyChange False >> KeyChange)
        , Window.resizes Resize
        ]


init : ( Model, Cmd Msg )
init =
    ( Model.model
    , Cmd.batch
        [ Texture.loadWith
            { defaultOptions
                | magnify = Texture.nearest
                , minify = Texture.nearest
            }
            "data:image/png;base64,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"
            |> Task.attempt TextureLoaded
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
