module Messages exposing (Msg(..))

import Window exposing (Size)
import Time exposing (Time)
import WebGL.Texture exposing (Error, Texture)
import Keyboard exposing (KeyCode)


type Msg
    = Resize Size
    | Animate Time
    | KeyChange Bool KeyCode
    | TextureLoaded (Result Error Texture)
    | FontLoaded (Result Error Texture)
