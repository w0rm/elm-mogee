module View.Sprite exposing
    ( Sprite
    , fill
    , loadSprite
    , name
    , render
    , renderTransformed
    , renderWith
    , sprite
    )

import Dict
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Task
import View.Common exposing (Vertex, box, cropMask, writeMask)
import View.SpriteData as SpriteData exposing (SpriteInfo, spriteSrc)
import WebGL exposing (Entity, Shader)
import WebGL.Settings exposing (Setting)
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Texture as Texture exposing (Error, Texture, defaultOptions)


type Sprite
    = Sprite String SpriteInfo


{-| TODO: use union type for sprites
-}
sprite : String -> Sprite
sprite n =
    Dict.get n SpriteData.sprite
        |> Maybe.withDefault { x = 0, y = 0, w = 0, h = 0, rotate = 0 }
        |> Sprite n


name : Sprite -> String
name (Sprite n _) =
    n


renderWith : List Setting -> Sprite -> Texture -> Bool -> ( Float, Float, Float ) -> Entity
renderWith settings (Sprite _ { x, y, w, h, rotate }) texture mirror ( dx, dy, dz ) =
    WebGL.entityWith settings
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = vec3 dx dy dz
        , texture = texture
        , size = vec2 w h
        , mirror =
            if mirror then
                1

            else
                0
        , rotate = rotate
        , transform = Mat4.identity
        , frameSize =
            if rotate == 1 then
                vec2 h w

            else
                vec2 w h
        , textureOffset = vec2 x y
        , textureSize =
            vec2
                (toFloat (Tuple.first (Texture.size texture)))
                (toFloat (Tuple.second (Texture.size texture)))
        }


renderTransformed : Sprite -> Texture -> Mat4 -> ( Float, Float, Float ) -> Entity
renderTransformed (Sprite _ { x, y, w, h, rotate }) texture transform ( dx, dy, dz ) =
    WebGL.entityWith [ writeMask 0, DepthTest.default ]
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = vec3 dx dy dz
        , texture = texture
        , size = vec2 w h
        , mirror = 0
        , transform = transform
        , rotate = rotate
        , frameSize =
            if rotate == 1 then
                vec2 h w

            else
                vec2 w h
        , textureOffset = vec2 x y
        , textureSize =
            vec2
                (toFloat (Tuple.first (Texture.size texture)))
                (toFloat (Tuple.second (Texture.size texture)))
        }


render : Sprite -> Texture -> ( Float, Float, Float ) -> Entity
render (Sprite _ { x, y, w, h, rotate }) texture ( dx, dy, dz ) =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = vec3 dx dy dz
        , texture = texture
        , size = vec2 w h
        , rotate = rotate
        , mirror = 0
        , transform = Mat4.identity
        , frameSize =
            if rotate == 1 then
                vec2 h w

            else
                vec2 w h
        , textureOffset = vec2 x y
        , textureSize =
            vec2
                (toFloat (Tuple.first (Texture.size texture)))
                (toFloat (Tuple.second (Texture.size texture)))
        }


fill : Sprite -> Texture -> ( Float, Float ) -> ( Float, Float, Float ) -> Entity
fill (Sprite _ { x, y, w, h, rotate }) texture ( width, height ) ( dx, dy, dz ) =
    WebGL.entityWith
        [ cropMask 1, DepthTest.default ]
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = vec3 dx dy dz
        , texture = texture
        , size = vec2 width height
        , rotate = rotate
        , mirror = 0
        , transform = Mat4.identity
        , frameSize =
            if rotate == 1 then
                vec2 h w

            else
                vec2 w h
        , textureOffset = vec2 x y
        , textureSize =
            vec2
                (toFloat (Tuple.first (Texture.size texture)))
                (toFloat (Tuple.second (Texture.size texture)))
        }


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
    , rotate : Int
    , transform : Mat4
    , mirror : Int
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
        uniform int rotate;
        uniform int mirror;
        uniform mat4 transform;
        varying vec2 texturePos;

        void main () {
            vec2 newPosition = position;
            vec2 newSize = size;
            if (rotate == 1) {
                newPosition = vec2(1.0-position.y, position.x);
                newSize = vec2(size.y,size.x);
            }
            if (mirror == 1) {
                newPosition = vec2(1.0-newPosition.x, newPosition.y);
            }
            vec2 clipSpace = vec2(transform * vec4(newPosition * size, 0, 1.0)) + offset.xy - 32.0;
            gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
            texturePos = position * newSize;
        }

    |]


texturedFragmentShader : Shader {} Uniform Varying
texturedFragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        uniform vec2 textureSize;
        uniform vec2 textureOffset;
        uniform vec2 frameSize;
        varying vec2 texturePos;

        void main () {
          vec2 pos = vec2(
            float(int(texturePos.x) - int(texturePos.x) / int(frameSize.x) * int(frameSize.x)),
            float(int(texturePos.y) - int(texturePos.y) / int(frameSize.y) * int(frameSize.y))
          );
          vec2 offset = (pos + textureOffset) / textureSize;
          gl_FragColor = texture2D(texture, offset);
          if (gl_FragColor.a == 0.0) discard;
        }

    |]
