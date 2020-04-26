module View.Font exposing
    ( Text
    , load
    , name
    , render
    , text
    )

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import MogeeFont
import Task
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture, defaultOptions)


type Text
    = Text String (Mesh Vertex)


name : Text -> String
name (Text n _) =
    n


type alias Vertex =
    { position : Vec2
    , texPosition : Vec2
    }


load : (Result Error Texture -> msg) -> Cmd msg
load msg =
    Texture.loadWith
        { defaultOptions
            | magnify = Texture.nearest
            , minify = Texture.nearest
            , flipY = False
        }
        MogeeFont.spriteSrc
        |> Task.attempt msg


render : Vec3 -> Text -> Texture -> ( Float, Float, Float ) -> Entity
render color (Text _ mesh) texture ( x, y, z ) =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        mesh
        { offset = vec3 x y z
        , texture = texture
        , color = color
        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
        }


text : String -> Text
text string =
    Text string (WebGL.triangles (MogeeFont.text addLetter string))


addLetter : MogeeFont.Letter -> List ( Vertex, Vertex, Vertex )
addLetter { x, y, width, height, textureX, textureY } =
    [ ( Vertex (vec2 x y) (vec2 textureX textureY)
      , Vertex (vec2 (x + width) (y + height)) (vec2 (textureX + width) (textureY + height))
      , Vertex (vec2 (x + width) y) (vec2 (textureX + width) textureY)
      )
    , ( Vertex (vec2 x y) (vec2 textureX textureY)
      , Vertex (vec2 x (y + height)) (vec2 textureX (textureY + height))
      , Vertex (vec2 (x + width) (y + height)) (vec2 (textureX + width) (textureY + height))
      )
    ]



-- Shaders


type alias Uniform =
    { offset : Vec3
    , color : Vec3
    , texture : Texture
    , textureSize : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


texturedVertexShader : Shader Vertex Uniform Varying
texturedVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        attribute vec2 texPosition;
        uniform vec2 textureSize;
        uniform vec3 offset;
        varying vec2 texturePos;

        void main () {
            vec2 clipSpace = position + offset.xy - 32.0;
            gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
            texturePos = texPosition / textureSize;
        }

    |]


texturedFragmentShader : Shader {} Uniform Varying
texturedFragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        uniform vec3 color;
        varying vec2 texturePos;

        void main () {
            vec4 textureColor = texture2D(texture, texturePos);
            gl_FragColor = vec4(color, 1.0);
            if (dot(textureColor, textureColor) == 4.0) discard;
        }

    |]
