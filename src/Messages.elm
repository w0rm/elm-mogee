module Messages exposing (Msg(..))

import Window exposing (Size)
import Time exposing (Time)
import Model.Keys exposing (Keys)
import WebGL.Texture exposing (Error, Texture)


type Msg
    = Resize Size
    | Animate Time
    | KeyChange (Keys -> Keys)
    | TextureLoaded (Result Error Texture)
