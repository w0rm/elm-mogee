module View.Font
    exposing
        ( text
        , render
        , Text
        , load
        )

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture exposing (Error, defaultOptions)
import Dict exposing (Dict)
import String
import Task


font : Dict Char CharInfo
font =
    Dict.fromList
        [ ( '(', CharInfo 0 0 3 )
        , ( ')', CharInfo 3 0 3 )
        , ( '*', CharInfo 6 0 3 )
        , ( '0', CharInfo 9 0 3 )
        , ( '1', CharInfo 12 0 2 )
        , ( '2', CharInfo 14 0 3 )
        , ( '3', CharInfo 17 0 3 )
        , ( '4', CharInfo 20 0 3 )
        , ( '5', CharInfo 23 0 3 )
        , ( '6', CharInfo 26 0 3 )
        , ( '7', CharInfo 29 0 3 )
        , ( '8', CharInfo 32 0 3 )
        , ( '9', CharInfo 35 0 3 )
        , ( '[', CharInfo 38 0 2 )
        , ( ']', CharInfo 40 0 2 )
        , ( 'a', CharInfo 42 0 3 )
        , ( 'b', CharInfo 45 0 3 )
        , ( 'c', CharInfo 48 0 3 )
        , ( 'd', CharInfo 51 0 3 )
        , ( 'e', CharInfo 54 0 3 )
        , ( 'f', CharInfo 57 0 4 )
        , ( 'g', CharInfo 61 0 3 )
        , ( 'h', CharInfo 0 11 3 )
        , ( 'i', CharInfo 3 11 1 )
        , ( 'j', CharInfo 4 11 3 )
        , ( 'k', CharInfo 7 11 3 )
        , ( 'l', CharInfo 10 11 1 )
        , ( 'm', CharInfo 11 11 5 )
        , ( 'n', CharInfo 16 11 3 )
        , ( 'o', CharInfo 19 11 3 )
        , ( 'p', CharInfo 22 11 3 )
        , ( 'q', CharInfo 25 11 3 )
        , ( 'r', CharInfo 28 11 3 )
        , ( 's', CharInfo 31 11 3 )
        , ( 't', CharInfo 34 11 3 )
        , ( 'u', CharInfo 37 11 3 )
        , ( 'v', CharInfo 40 11 3 )
        , ( 'w', CharInfo 43 11 5 )
        , ( 'x', CharInfo 48 11 3 )
        , ( 'y', CharInfo 51 11 3 )
        , ( 'z', CharInfo 54 11 3 )
        , ( '{', CharInfo 57 11 3 )
        , ( '}', CharInfo 60 11 3 )
        ]


fontSrc : String
fontSrc =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABAAQAAAACCEkxzAAAAzUlEQVR4nGP8zwABTBcYGBj+2vx5wfRiNqchZwPDDyYG9ph9Luem6zJWbtLicBGcLsjCWG++ZIem/Cem//XcfH8EHjYzbhcMZXs1nW8lw/b/////3f//DsP5/////7H/f48RavItxr9MUCsKHzAwMDAwvGDiY2BgeMDA8IVlA+vi/HyPf8JMCpyqE28wMDMw+vz7f0f3R4IwE4MDA5MDw3w5JgYGTvlL2ewNDH///33////v20zfIObxMPz///f9///P/zPCfcEwyqArAwBEIUx+get51wAAAABJRU5ErkJggg=="


type alias CharInfo =
    { x : Float
    , y : Float
    , w : Float
    }


fontHeight : Float
fontHeight =
    11


spaceWidth : Float
spaceWidth =
    3


type Text
    = Text (Mesh Vertex)


kerning : Dict ( Char, Char ) Float
kerning =
    Dict.fromList
        []


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
        fontSrc
        |> Task.attempt msg


render : ( Float, Float, Float ) -> Text -> Texture -> ( Float, Float, Float ) -> Entity
render color (Text mesh) texture offset =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        mesh
        { offset = Vec3.fromTuple offset
        , texture = texture
        , color = Vec3.fromTuple color
        , textureOffset = vec2 0 0
        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
        }


text : String -> Text
text text =
    Text (WebGL.triangles (textMeshHelper Nothing text 0 0 []))


textMeshHelper : Maybe Char -> String -> Float -> Float -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
textMeshHelper prevChar text currentX currentY list =
    case String.uncons text of
        Just ( ' ', rest ) ->
            textMeshHelper (Just ' ') rest (currentX + spaceWidth) currentY list

        Just ( '\n', rest ) ->
            textMeshHelper (Just '\n') rest 0 (currentY + fontHeight) list

        Just ( char, rest ) ->
            case Dict.get char font of
                Just charInfo ->
                    addLetter charInfo ( currentX + (letterSpacing prevChar char), currentY ) <|
                        textMeshHelper (Just char) rest (currentX + charInfo.w + (letterSpacing prevChar char)) currentY list

                Nothing ->
                    textMeshHelper prevChar rest currentX currentY list

        Nothing ->
            list


letterSpacing : Maybe Char -> Char -> Float
letterSpacing prevChar nextChar =
    case prevChar of
        Nothing ->
            0

        Just ' ' ->
            0

        Just '\n' ->
            0

        Just char ->
            Dict.get ( char, nextChar ) kerning
                |> Maybe.withDefault 1


addLetter : CharInfo -> ( Float, Float ) -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
addLetter char ( x, y ) =
    [ ( Vertex (vec2 x y) (vec2 char.x char.y)
      , Vertex (vec2 (x + char.w) (y + fontHeight)) (vec2 (char.x + char.w) (char.y + fontHeight))
      , Vertex (vec2 (x + char.w) y) (vec2 (char.x + char.w) char.y)
      )
    , ( Vertex (vec2 x y) (vec2 char.x char.y)
      , Vertex (vec2 x (y + fontHeight)) (vec2 char.x (char.y + fontHeight))
      , Vertex (vec2 (x + char.w) (y + fontHeight)) (vec2 (char.x + char.w) (char.y + fontHeight))
      )
    ]
        |> (++)



-- Shaders


type alias Uniform =
    { offset : Vec3
    , color : Vec3
    , texture : Texture
    , textureSize : Vec2
    , textureOffset : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


texturedVertexShader : Shader Vertex Uniform Varying
texturedVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        attribute vec2 texPosition;
        uniform vec3 offset;
        varying vec2 texturePos;

        void main () {
            vec2 roundOffset = vec2(floor(offset.x + 0.5), floor(offset.y + 0.5));
            vec2 clipSpace = position + roundOffset - 32.0;
            gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
            texturePos = texPosition;
        }

    |]


texturedFragmentShader : Shader {} Uniform Varying
texturedFragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        uniform vec2 textureSize;
        uniform vec2 textureOffset;
        uniform vec3 color;
        varying vec2 texturePos;

        void main () {
          vec2 offset = (texturePos + textureOffset) / textureSize;
          vec4 textureColor = texture2D(texture, offset);
          gl_FragColor = vec4(color, 1.0);
          if (textureColor.r == 1.0) discard;
        }

    |]
