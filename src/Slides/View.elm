module Slides.View exposing (render)

import Animation exposing (Animation)
import Components.Components as Components
import Dict
import Slides.Engine as Engine exposing (Element(..), Engine)
import View.Color as Color
import View.Font as Font exposing (Text)
import View.Sprite as Sprite exposing (Sprite)
import WebGL exposing (Entity)
import WebGL.Texture exposing (Texture)


roundFloat : Float -> Float
roundFloat =
    round >> toFloat


render : Texture -> Texture -> Engine -> List Entity
render sprite font { animations, elements, time } =
    Dict.foldr
        (\elementId { position, element } ->
            case Dict.get elementId animations of
                Just { x, y } ->
                    (::) (renderElement sprite font element (Animation.animate time x) (Animation.animate time y))

                Nothing ->
                    (::) (renderElement sprite font element position.x position.y)
        )
        []
        elements


renderElement : Texture -> Texture -> Element -> Float -> Float -> Entity
renderElement sprite font element x y =
    case element of
        SpriteElement spriteElement ->
            Sprite.render spriteElement sprite ( x |> roundFloat, y |> roundFloat, 0 )

        TextElement textElement ->
            Font.render Color.white textElement font ( x |> roundFloat, y |> roundFloat, 0 )
