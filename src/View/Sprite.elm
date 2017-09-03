module View.Sprite
    exposing
        ( loadTexture
        , loadSprite
        , sprite
        , name
        , Sprite
        , render
        )

import View.SpriteData as SpriteData exposing (textureSrc, spriteSrc, SpriteInfo)
import WebGL.Texture as Texture exposing (Error, defaultOptions)
import Task
import Dict
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Texture, Shader, Mesh, Entity)


type Sprite
    = Sprite String (Mesh Vertex)


{-| TODO: use union type for sprites
-}
sprite : String -> Sprite
sprite name =
    Dict.get name SpriteData.sprite
        |> Maybe.map (mesh)
        |> Maybe.withDefault []
        |> WebGL.triangles
        |> Sprite name


name : Sprite -> String
name (Sprite name _) =
    name


type alias Vertex =
    { position : Vec2
    , texPosition : Vec2
    }


mesh : SpriteInfo -> List ( Vertex, Vertex, Vertex )
mesh { x, y, w, h } =
    [ ( Vertex (vec2 0 0) (vec2 x y)
      , Vertex (vec2 w h) (vec2 (x + w) (y + h))
      , Vertex (vec2 w 0) (vec2 (x + w) y)
      )
    , ( Vertex (vec2 0 0) (vec2 x y)
      , Vertex (vec2 0 h) (vec2 x (y + h))
      , Vertex (vec2 w h) (vec2 (x + w) (y + h))
      )
    ]


render : Sprite -> Texture -> ( Float, Float, Float ) -> Entity
render (Sprite _ mesh) texture offset =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        mesh
        { offset = Vec3.fromTuple offset
        , texture = texture
        , textureSize =
            vec2
                (toFloat (Tuple.first (Texture.size texture)))
                (toFloat (Tuple.second (Texture.size texture)))
        }


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


loadSprite : (Result Error Texture -> msg) -> Cmd msg
loadSprite msg =
    Texture.loadWith
        { defaultOptions
            | magnify = Texture.nearest
            , minify = Texture.nearest
            , flipY = False
        }
        spriteSrc
        |> Task.attempt msg



-- Shaders


type alias Uniform =
    { offset : Vec3
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
        varying vec2 texturePos;

        void main () {
            gl_FragColor = texture2D(texture, texturePos);
            if (gl_FragColor.a == 0.0) discard;
        }

    |]
