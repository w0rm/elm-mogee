module Messages exposing (Msg(..))

import Window exposing (Size)
import Time exposing (Time)
import WebGL.Texture exposing (Error, Texture)
import Keyboard exposing (KeyCode)
import PageVisibility exposing (Visibility)


type Msg
    = Resize Size
    | Animate Time
    | KeyChange Bool KeyCode
    | VisibilityChange Visibility
    | TextureLoaded (Result Error Texture)
    | SpriteLoaded (Result Error Texture)
    | FontLoaded (Result Error Texture)
