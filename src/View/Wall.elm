module View.Wall exposing (render)

import Components.Transform exposing (Transform)
import View.Sprite as Sprite
import WebGL exposing (Entity)
import WebGL.Texture exposing (Texture)


render : Texture -> Transform -> Entity
render sprite { x, y, width, height } =
    Sprite.fill
        (Sprite.sprite "wall")
        sprite
        -- only expand wider walls
        ( width
        , if width == 1 || height == 1 then
            height

          else
            height + 3
        )
        ( x, y, 3 )
