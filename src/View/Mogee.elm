module View.Mogee exposing (render)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Model.Mogee exposing (Mogee, size, AnimationState(..))
import View.Common exposing (box, texturedFragmentShader)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture


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


render : Texture -> ( Float, Float ) -> Float -> Mogee -> Entity
render texture ( x, y ) directionX mogee =
    let
        layer =
            if mogee.state == Dead then
                1
            else
                4

        mirror =
            if directionX < 0 then
                -1
            else
                1
    in
        WebGL.entity
            texturedVertexShader
            texturedFragmentShader
            box
            { offset = Vec3.fromTuple ( toFloat (round x), toFloat (round y), layer )
            , texture = texture
            , mirror = mirror
            , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
            , frameSize = Vec2.fromTuple size
            , textureOffset = vec2 (Tuple.first size * getFrame mogee.frames) 0
            }



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
