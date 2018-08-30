module Messages exposing (Msg(..))

import Browser.Events exposing (Visibility(..))
import WebGL.Texture exposing (Error, Texture)


type Msg
    = Resize Int Int
    | Animate Float
    | KeyChange Bool Int
    | VisibilityChange Visibility
    | TextureLoaded (Result Error Texture)
    | SpriteLoaded (Result Error Texture)
    | FontLoaded (Result Error Texture)
