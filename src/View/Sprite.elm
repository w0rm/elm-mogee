module View.Sprite exposing
    ( Sprite
    , fill
    , loadSprite
    , loadTexture
    , name
    , render
    , sprite
    )

import Dict
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import View.Common exposing (Vertex, box, cropMask, texturedFragmentShader)
import View.SpriteData as SpriteData exposing (SpriteInfo, spriteSrc, textureSrc)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Texture as Texture exposing (Error, Texture, defaultOptions)


type Sprite
    = Sprite String SpriteInfo


{-| TODO: use union type for sprites
-}
sprite : String -> Sprite
sprite n =
    Dict.get n SpriteData.sprite
        |> Maybe.withDefault { x = 0, y = 0, w = 0, h = 0 }
        |> Sprite n


name : Sprite -> String
name (Sprite n _) =
    n


render : Sprite -> Texture -> ( Float, Float, Float ) -> Entity
render (Sprite _ { x, y, w, h }) texture ( dx, dy, dz ) =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = vec3 dx dy dz
        , texture = texture
        , size = vec2 w h
        , frameSize = vec2 w h
        , textureOffset = vec2 x y
        , textureSize =
            vec2
                (toFloat (Tuple.first (Texture.size texture)))
                (toFloat (Tuple.second (Texture.size texture)))
        }


fill : Sprite -> Texture -> ( Float, Float ) -> ( Float, Float, Float ) -> Entity
fill (Sprite _ { x, y, w, h }) texture ( width, height ) ( dx, dy, dz ) =
    WebGL.entityWith
        [ cropMask 1, DepthTest.default ]
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = vec3 dx dy dz
        , texture = texture
        , size = vec2 width height
        , frameSize = vec2 w h
        , textureOffset = vec2 x y
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
    , size : Vec2
    , frameSize : Vec2
    , textureOffset : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


texturedVertexShader : Shader Vertex Uniform Varying
texturedVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        uniform vec2 size;
        uniform vec3 offset;
        varying vec2 texturePos;

        void main () {
            vec2 clipSpace = position * size + offset.xy - 32.0;
            gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
            texturePos = position * size;
        }

    |]
