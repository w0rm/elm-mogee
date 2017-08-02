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
        [ ( '!', CharInfo 0 0 1 )
        , ( '"', CharInfo 1 0 3 )
        , ( '\'', CharInfo 4 0 1 )
        , ( '(', CharInfo 5 0 3 )
        , ( ')', CharInfo 8 0 3 )
        , ( '+', CharInfo 11 0 3 )
        , ( ',', CharInfo 14 0 2 )
        , ( '-', CharInfo 16 0 3 )
        , ( '.', CharInfo 19 0 1 )
        , ( '/', CharInfo 20 0 3 )
        , ( '0', CharInfo 23 0 3 )
        , ( '1', CharInfo 26 0 2 )
        , ( '2', CharInfo 28 0 3 )
        , ( '3', CharInfo 31 0 3 )
        , ( '4', CharInfo 34 0 3 )
        , ( '5', CharInfo 37 0 3 )
        , ( '6', CharInfo 40 0 3 )
        , ( '7', CharInfo 43 0 3 )
        , ( '8', CharInfo 46 0 3 )
        , ( '9', CharInfo 49 0 3 )
        , ( ':', CharInfo 52 0 1 )
        , ( ';', CharInfo 53 0 2 )
        , ( '<', CharInfo 55 0 5 )
        , ( '=', CharInfo 60 0 3 )
        , ( '>', CharInfo 0 11 5 )
        , ( '?', CharInfo 5 11 3 )
        , ( '[', CharInfo 8 11 2 )
        , ( ']', CharInfo 10 11 2 )
        , ( '_', CharInfo 12 11 4 )
        , ( 'a', CharInfo 16 11 3 )
        , ( 'b', CharInfo 19 11 3 )
        , ( 'c', CharInfo 22 11 3 )
        , ( 'd', CharInfo 25 11 3 )
        , ( 'e', CharInfo 28 11 3 )
        , ( 'f', CharInfo 31 11 4 )
        , ( 'g', CharInfo 35 11 3 )
        , ( 'h', CharInfo 38 11 3 )
        , ( 'i', CharInfo 41 11 1 )
        , ( 'j', CharInfo 42 11 3 )
        , ( 'k', CharInfo 45 11 3 )
        , ( 'l', CharInfo 48 11 1 )
        , ( 'm', CharInfo 49 11 5 )
        , ( 'n', CharInfo 54 11 3 )
        , ( 'o', CharInfo 57 11 3 )
        , ( 'p', CharInfo 60 11 3 )
        , ( 'q', CharInfo 0 22 3 )
        , ( 'r', CharInfo 3 22 3 )
        , ( 's', CharInfo 6 22 3 )
        , ( 't', CharInfo 9 22 3 )
        , ( 'u', CharInfo 12 22 3 )
        , ( 'v', CharInfo 15 22 3 )
        , ( 'w', CharInfo 18 22 5 )
        , ( 'x', CharInfo 23 22 3 )
        , ( 'y', CharInfo 26 22 3 )
        , ( 'z', CharInfo 29 22 3 )
        , ( '{', CharInfo 32 22 3 )
        , ( '|', CharInfo 35 22 1 )
        , ( '}', CharInfo 36 22 3 )
        , ( '×', CharInfo 39 22 3 )
        ]


fontSrc : String
fontSrc =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABAAQAAAACCEkxzAAABBklEQVR4nO2NTStEYRhAz/t0aRbUJIspZHzEXZqFKA2vmo0/QMlCdjYsrJQyKdZ+wdCUKBv+gBllQSIrTUw+Shk1i8vmjmbufSzurOY3OKuzOodjjWBkOxJZsMFq6jKJA/RN/cYfnDARuicD8ytCNRnOHnzdm3q5//z5NlaWnc/vzZ+ZSo7gQlW1rmIKAA5GiRAAaiBB3PNyV1kkXIa3SXAyqYnByvhut7nLLy5lbvyGuNgtFOQJu9dhQNaYy9dgGF1X3fDVFw4hVgLhHapjtLyuAaEIH4Az+rLf6AFk6Ix2LYJjPFNIdFmkLavTaQuSPnrsNBZ4VfVPVVUAepvBkkvr4l+a/AFm32liij5ttQAAAABJRU5ErkJggg=="


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
