module View.Sprite exposing (loadTexture)

import View.SpriteData exposing (textureSrc)
import WebGL exposing (Texture)
import WebGL.Texture as Texture exposing (Error, defaultOptions)
import Task


loadTexture : (Result Error Texture -> msg) -> Cmd msg
loadTexture msg =
    Texture.loadWith
        { defaultOptions
            | magnify = Texture.nearest
            , minify = Texture.nearest
            , flipY = False
        }
        textureSrc
        |> Task.attempt msg
