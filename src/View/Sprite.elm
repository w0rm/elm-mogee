module View.Sprite
    exposing
        ( loadTexture
        , loadSprite
        , sprite
        , name
        , Sprite
        , render
        , fill
        )

import View.SpriteData as SpriteData exposing (textureSrc, spriteSrc, SpriteInfo)
import WebGL.Texture as Texture exposing (Error, defaultOptions)
import Task
import Dict
import View.Common exposing (box, Vertex, texturedFragmentShader, cropMask)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Settings.DepthTest as DepthTest


type Sprite
    = Sprite String SpriteInfo


{-| TODO: use union type for sprites
-}
sprite : String -> Sprite
sprite name =
    Dict.get name SpriteData.sprite
        |> Maybe.withDefault { x = 0, y = 0, w = 0, h = 0 }
        |> Sprite name


name : Sprite -> String
name (Sprite name _) =
    name


render : Sprite -> Texture -> ( Float, Float, Float ) -> Entity
render (Sprite _ { x, y, w, h }) texture offset =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple offset
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
fill (Sprite _ { x, y, w, h }) texture size offset =
    WebGL.entityWith
        [ cropMask 1, DepthTest.default ]
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple offset
        , texture = texture
        , size = Vec2.fromTuple size
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
