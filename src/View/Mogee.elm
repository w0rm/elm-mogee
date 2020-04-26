module View.Mogee exposing (render, renderBg)

import Components.Mogee exposing (AnimationState(..), Mogee)
import Components.Transform exposing (Transform)
import View.Common exposing (cropMask)
import View.Sprite as Sprite
import WebGL exposing (Entity)
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Texture exposing (Texture)


getFrame : List Int -> Int
getFrame frames =
    List.head frames |> Maybe.withDefault 0


renderBg : Texture -> ( Float, Float ) -> List Entity -> List Entity
renderBg sprite cameraOffset =
    (::) (Sprite.fill (Sprite.sprite "background") sprite ( 128 * 2, 256 ) (bgOffset cameraOffset))


render : Texture -> Float -> Mogee -> Transform -> List Entity -> List Entity
render sprite directionX mogee { x, y } =
    (++)
        [ Sprite.renderWith [ DepthTest.default, cropMask 1 ]
            (Sprite.sprite ("mogee-" ++ String.fromInt (getFrame mogee.frames)))
            sprite
            (directionX < 0)
            ( x, y, 4 )
        , Sprite.renderWith [ DepthTest.default, cropMask 0 ]
            (Sprite.sprite "mogee-7")
            sprite
            False
            ( x, y, 1 )
        ]


bgOffset : ( Float, Float ) -> ( Float, Float, Float )
bgOffset ( cameraX, cameraY ) =
    ( toFloat -(modBy 128 (round (cameraX / 2)))
    , toFloat -(round (cameraY / 4 + 128) |> max 0 |> min 192)
    , 5
    )
