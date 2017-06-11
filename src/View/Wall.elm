module View.Wall exposing (render)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import View.Common exposing (box, texturedFragmentShader)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture


type alias UniformTextured =
    { size : Vec2
    , offset : Vec3
    , texture : Texture
    , textureSize : Vec2
    , frameSize : Vec2
    , textureOffset : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


render : Texture -> ( Float, Float ) -> ( Float, Float, Float ) -> Entity
render texture ( w, h ) position =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple position
        , texture = texture
        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
        , textureOffset = vec2 0 10
        , frameSize = vec2 64 5
        , size =
            vec2 w
                (if w == 1 || h == 1 then
                    h
                 else
                    h + 3
                )

        -- only expand wider walls
        }



-- Shaders


texturedVertexShader : Shader View.Common.Vertex UniformTextured Varying
texturedVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        uniform vec3 offset;
        uniform vec2 size;
        varying vec2 texturePos;

        void main () {
          vec2 roundOffset = vec2(floor(offset.x + 0.5), floor(offset.y + 0.5));
          vec2 clipSpace = position * size + roundOffset - 32.0;
          gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
          texturePos = position * size;
        }

    |]
