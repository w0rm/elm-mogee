module View.Lives
    exposing
        ( renderLives
        , renderScore
        , renderTitle
        , renderPlay
        )

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import View.Common exposing (box, texturedFragmentShader)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture
import View.Font as Font exposing (Text)


type alias UniformTextured =
    { offset : Vec3
    , frameSize : Vec2
    , textureOffset : Vec2
    , texture : Texture
    , textureSize : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


renderLives : Texture -> ( Float, Float, Float ) -> Int -> List Entity
renderLives texture ( x, y, z ) lives =
    List.map (\i -> renderLive texture ( x + toFloat i * 6, y, z )) (List.range 0 (lives - 1))


digitsList : Int -> List Int
digitsList n =
    let
        nn =
            n // 10

        r =
            n % 10
    in
        if nn == 0 && r == 0 then
            []
        else
            r :: digitsList nn


renderScore : Texture -> ( Float, Float, Float ) -> Int -> List Entity
renderScore texture ( x, y, z ) value =
    let
        digits =
            digitsList value |> List.reverse

        position =
            ( x - toFloat (List.length digits) * 2, y, z )
    in
        List.indexedMap (renderDigit texture position) digits


renderDigit : Texture -> ( Float, Float, Float ) -> Int -> Int -> Entity
renderDigit texture ( x, y, z ) index number =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple ( x + toFloat index * 4, y, z )
        , texture = texture
        , textureOffset = vec2 (toFloat number * 3) 50
        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
        , frameSize = vec2 3 4
        }


renderLive : Texture -> ( Float, Float, Float ) -> Entity
renderLive texture offset =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple offset
        , texture = texture
        , textureOffset = vec2 56 0
        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
        , frameSize = vec2 5 6
        }


renderTitle : Texture -> ( Float, Float ) -> Entity
renderTitle texture ( x, y ) =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple ( x, y, 0 )
        , texture = texture
        , textureOffset = vec2 0 15
        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
        , frameSize = vec2 58 24
        }


playText : Text
playText =
    Font.text "press enter\nto play"


renderPlay : Texture -> ( Float, Float, Float ) -> Entity
renderPlay =
    Font.render ( 1, 1, 1 ) playText



-- Shaders


texturedVertexShader : Shader View.Common.Vertex UniformTextured Varying
texturedVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        uniform vec3 offset;
        uniform vec2 frameSize;
        varying vec2 texturePos;

        void main () {
            vec2 roundOffset = vec2(floor(offset.x + 0.5), floor(offset.y + 0.5));
            vec2 clipSpace = position * frameSize + roundOffset - 32.0;
            gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
            texturePos = position * frameSize;
        }

    |]
