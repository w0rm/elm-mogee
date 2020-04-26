module View.Mogee exposing (render, renderBg)

import Components.Mogee as Mogee exposing (AnimationState(..), Mogee)
import Components.Transform exposing (Transform)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import View.Common exposing (box, cropMask, texturedFragmentShader)
import View.Sprite as Sprite exposing (Sprite)
import WebGL exposing (Entity, Shader)
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Texture as Texture exposing (Texture)


type alias UniformTextured =
    { frameSize : Vec2
    , offset : Vec3
    , mirror : Float
    , texture : Texture
    , textureSize : Vec2
    , textureOffset : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


getFrame : List Float -> Float
getFrame frames =
    List.head frames |> Maybe.withDefault 0


bg : Sprite
bg =
    Sprite.sprite "background"


renderBg : Texture -> ( Float, Float ) -> List Entity -> List Entity
renderBg sprite cameraOffset =
    (::) (Sprite.fill bg sprite ( 128 * 2, 256 ) (bgOffset cameraOffset))


render : Texture -> Float -> Mogee -> Transform -> List Entity -> List Entity
render texture directionX mogee { x, y } =
    let
        mirror =
            if directionX < 0 then
                -1

            else
                1
    in
    (++)
        [ WebGL.entityWith [ DepthTest.default, cropMask 1 ]
            texturedVertexShader
            texturedFragmentShader
            box
            { offset = vec3 x y 4
            , texture = texture
            , mirror = mirror
            , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
            , frameSize = vec2 Mogee.width Mogee.height
            , textureOffset = vec2 (Mogee.width * getFrame mogee.frames) 0
            }
        , WebGL.entityWith [ DepthTest.default, cropMask 0 ]
            texturedVertexShader
            texturedFragmentShader
            box
            { offset = vec3 x y 1
            , texture = texture
            , mirror = mirror
            , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
            , frameSize = vec2 Mogee.width Mogee.height
            , textureOffset = vec2 (Mogee.width * 7) 0
            }
        ]


bgOffset : ( Float, Float ) -> ( Float, Float, Float )
bgOffset ( cameraX, cameraY ) =
    ( toFloat -(modBy 128 (round (cameraX / 2)))
    , toFloat -(round (cameraY / 4 + 128) |> max 0 |> min 192)
    , 5
    )



-- Shaders


texturedVertexShader : Shader View.Common.Vertex UniformTextured Varying
texturedVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        uniform vec3 offset;
        uniform float mirror;
        uniform vec2 frameSize;
        varying vec2 texturePos;

        void main () {
          vec2 clipSpace = position * frameSize + offset.xy - 32.0;
          gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
          texturePos = vec2((position.x - 0.5) * mirror + 0.5, position.y) * frameSize;
        }

    |]
