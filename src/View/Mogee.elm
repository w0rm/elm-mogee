module View.Mogee exposing (render)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Model.Mogee exposing (Mogee, size, AnimationState(..))
import View.Common exposing (box)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture


type alias UniformTextured =
    { frame : Float
    , frameSize : Vec2
    , offset : Vec3
    , mirror : Float
    , texture : Texture
    , textureSize : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


render : Texture -> ( Float, Float ) -> Mogee -> Float -> Entity
render texture position mogee mirror =
    let
        layer =
            if mogee.state == Dead then
                1
            else
                4
    in
        WebGL.entity
            texturedVertexShader
            texturedFragmentShader
            box
            { offset = Vec3.fromTuple ( Tuple.first position, Tuple.second position, layer )
            , texture = texture
            , frame = List.head mogee.frames |> Maybe.withDefault 0
            , mirror = mirror
            , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
            , frameSize = Vec2.fromTuple size
            }



-- Shaders


texturedVertexShader : Shader View.Common.Vertex UniformTextured Varying
texturedVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        uniform vec3 offset;
        uniform vec2 frameSize;
        varying vec2 texturePos;

        void main () {
          vec2 roundOffset = vec2(floor(offset.x + 0.5), floor(offset.y + 0.5));
          vec2 clipSpace = (position * frameSize + roundOffset) / 32.0 - 1.0;
          gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z / 10.0, 1);
          texturePos = position;
        }

    |]


texturedFragmentShader : Shader {} UniformTextured Varying
texturedFragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        uniform vec2 textureSize;
        uniform vec2 frameSize;
        uniform float frame;
        uniform float mirror;
        varying vec2 texturePos;

        void main () {
          vec2 size = frameSize / textureSize;
          vec2 frameOffset = size * vec2((1.0 - mirror) / 2.0 + frame, 0);
          vec2 textureClipSpace = size * vec2(texturePos.x * mirror, texturePos.y) - 1.0;
          gl_FragColor = texture2D(texture, vec2(textureClipSpace.x, -textureClipSpace.y) + frameOffset);
          if (gl_FragColor.a == 0.0) discard;
        }

    |]
