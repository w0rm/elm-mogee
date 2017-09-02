module View.Lives
    exposing
        ( renderLives
        , renderScore
        )

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import View.Common exposing (box, texturedFragmentShader)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture
import View.Sprite as Sprite exposing (Sprite)


type alias UniformTextured =
    { offset : Vec3
    , frameSize : Vec2
    , textureOffset : Vec2
    , texture : Texture
    , textureSize : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


liveSprite : Sprite
liveSprite =
    Sprite.sprite "life"


renderLives : Texture -> ( Float, Float, Float ) -> Int -> List Entity
renderLives sprite ( x, y, z ) lives =
    List.map (\i -> Sprite.render liveSprite sprite ( x + toFloat i * 6, y, z )) (List.range 0 (lives - 1))


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
        if value == 0 then
            []
        else
            List.indexedMap (renderDigit texture position) digits


renderDigit : Texture -> ( Float, Float, Float ) -> Int -> Int -> Entity
renderDigit texture ( x, y, z ) index number =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple ( x + toFloat index * 4, y, z )
        , texture = texture
        , textureOffset = vec2 (toFloat number * 3) 15
        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
        , frameSize = vec2 3 4
        }



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
            vec2 clipSpace = position * frameSize + offset.xy - 32.0;
            gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
            texturePos = position * frameSize;
        }

    |]
