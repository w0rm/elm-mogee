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
        [ loadTexture
            "data:image/png;base64,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"
            |> Task.attempt TextureLoaded
        , loadTexture
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAp0lEQVR4nO3WUQrDIAwAUFN2/yu7n7nJ1JWOSiu8B/2o2qBJA0bOOaeOiEiDqcMiIk4JNMF29QaudnoCblzsrkdKn03nnJsDlFborSkt0vtmFO9utrrXv/t+7/AR8V5TnjK3iqYFjlZshSr/0iSgruSeuuKrJmEb/db1+6hFylhvfJWkxOge0Cz8817wSsJtM+EeMCvwqDUAAAAAAAAAAAAAAAAAmOgJA/tfHXyVBbYAAAAASUVORK5CYII="
            |> Task.attempt FontLoaded
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


loadTexture =
    -- openssl base64 < file.png | tr -d '\n' | pbcopy
    Texture.loadWith
        { defaultOptions
            | magnify = Texture.nearest
            , minify = Texture.nearest
            , flipY = False
        }
