module Messages exposing (Msg(..))

import Browser.Events exposing (Visibility(..))
import Components.Gamepad exposing (Gamepad)
import WebGL.Texture exposing (Error, Texture)


type Msg
    = Resize Int Int
    | Animate Float
    | KeyChange Bool Int
    | GamepadChange Gamepad
    | VisibilityChange Visibility
    | SpriteLoaded (Result Error Texture)
    | FontLoaded (Result Error Texture)
