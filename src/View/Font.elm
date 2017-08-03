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
import View.FontData exposing (font, fontSrc, CharInfo)


emHeight : Float
emHeight =
    11


spaceWidth : Float
spaceWidth =
    3


tracking : Float
tracking =
    1


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
            textMeshHelper (Just '\n') rest 0 (currentY + emHeight) list

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
                |> Maybe.withDefault tracking


addLetter : CharInfo -> ( Float, Float ) -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
addLetter char ( x, y ) =
    [ ( Vertex (vec2 x y) (vec2 char.x char.y)
      , Vertex (vec2 (x + char.w) (y + emHeight)) (vec2 (char.x + char.w) (char.y + emHeight))
      , Vertex (vec2 (x + char.w) y) (vec2 (char.x + char.w) char.y)
      )
    , ( Vertex (vec2 x y) (vec2 char.x char.y)
      , Vertex (vec2 x (y + emHeight)) (vec2 char.x (char.y + emHeight))
      , Vertex (vec2 (x + char.w) (y + emHeight)) (vec2 (char.x + char.w) (char.y + emHeight))
      )
    ]
        |> (++)



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
