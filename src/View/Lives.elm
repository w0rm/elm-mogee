module View.Lives
    exposing
        ( renderLives
        , renderScore
        , renderTitle
        , renderPlay
        )

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL as GL
import View.Common exposing (box)


type alias UniformTextured =
    { offset : Vec3
    , size : Vec2
    , textureOffset : Vec2
    , texture : GL.Texture
    , textureSize : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


renderLives : GL.Texture -> ( Float, Float, Float ) -> Int -> List GL.Renderable
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


renderScore : GL.Texture -> ( Float, Float, Float ) -> Int -> List GL.Renderable
renderScore texture ( x, y, z ) value =
    let
        digits =
            digitsList value |> List.reverse

        position =
            ( x - toFloat (List.length digits) * 2, y, z )
    in
        List.indexedMap (renderDigit texture position) digits


renderDigit : GL.Texture -> ( Float, Float, Float ) -> Int -> Int -> GL.Renderable
renderDigit texture ( x, y, z ) index number =
    GL.render
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple ( x + toFloat index * 4, y, z )
        , texture = texture
        , textureOffset = vec2 (toFloat number * 3) 50
        , textureSize = vec2 (toFloat (Tuple.first (GL.textureSize texture))) (toFloat (Tuple.second (GL.textureSize texture)))
        , size = vec2 3 4
        }


renderLive : GL.Texture -> ( Float, Float, Float ) -> GL.Renderable
renderLive texture offset =
    GL.render
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple offset
        , texture = texture
        , textureOffset = vec2 56 0
        , textureSize = vec2 (toFloat (Tuple.first (GL.textureSize texture))) (toFloat (Tuple.second (GL.textureSize texture)))
        , size = vec2 5 6
        }


renderTitle : GL.Texture -> ( Float, Float ) -> GL.Renderable
renderTitle texture ( x, y ) =
    GL.render
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple ( x, y, 0 )
        , texture = texture
        , textureOffset = vec2 0 15
        , textureSize = vec2 (toFloat (Tuple.first (GL.textureSize texture))) (toFloat (Tuple.second (GL.textureSize texture)))
        , size = vec2 58 24
        }


renderPlay : GL.Texture -> ( Float, Float, Float ) -> GL.Renderable
renderPlay texture offset =
    GL.render
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec3.fromTuple offset
        , texture = texture
        , textureOffset = vec2 0 39
        , textureSize = vec2 (toFloat (Tuple.first (GL.textureSize texture))) (toFloat (Tuple.second (GL.textureSize texture)))
        , size = vec2 54 11
        }



-- Shaders


texturedVertexShader : GL.Shader View.Common.Vertex UniformTextured Varying
texturedVertexShader =
    [glsl|

  precision mediump float;
  attribute vec2 position;
  uniform vec3 offset;
  uniform vec2 size;
  varying vec2 texturePos;

  void main () {
    vec2 roundOffset = vec2(floor(offset.x + 0.5), floor(offset.y + 0.5));
    vec2 clipSpace = (position * size + roundOffset) / 32.0 - 1.0;
    gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z / 10.0, 1);
    texturePos = position * size;
  }

|]


texturedFragmentShader : GL.Shader {} UniformTextured Varying
texturedFragmentShader =
    [glsl|

  precision mediump float;
  uniform sampler2D texture;
  uniform vec2 textureSize;
  varying vec2 texturePos;
  uniform vec2 textureOffset;

  void main () {
    vec2 textureClipSpace = texturePos / textureSize - 1.0;
    vec2 offset = textureOffset / textureSize;
    gl_FragColor = texture2D(texture, vec2(textureClipSpace.x + offset.x, -textureClipSpace.y - offset.y));
    if (gl_FragColor.a == 0.0) discard;
  }

|]
