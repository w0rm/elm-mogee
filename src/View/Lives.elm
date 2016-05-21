module View.Lives exposing
  ( render
  , renderScore
  , renderTitle
  , renderPlay
  )

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import WebGL as GL
import View.Common exposing (box)


type alias UniformTextured =
  { offset : Vec2
  , size : Vec2
  , textureOffset : Vec2
  , texture : GL.Texture
  , textureSize : Vec2
  }


type alias Varying =
  { texturePos : Vec2 }


render : GL.Texture -> (Float, Float) -> Int -> List (Int, GL.Renderable)
render texture (x, y) lives =
  List.map (\i -> renderLive texture (x + toFloat i * 6, y)) [0..lives - 1]


digitsList : Int -> List Int
digitsList n =
  let
    nn = n // 10
    r = n % 10
  in
    if nn == 0 && r == 0 then
      []
    else
      r :: digitsList nn


renderScore : GL.Texture -> (Float, Float) -> Int -> List (Int, GL.Renderable)
renderScore texture (x, y) value =
  let
    digits = digitsList value |> List.reverse
    position = (x - toFloat (List.length digits) * 2, y)
  in
    List.indexedMap (renderDigit texture position) digits


renderDigit : GL.Texture -> (Float, Float) -> Int -> Int -> (Int, GL.Renderable)
renderDigit texture (x, y) index number =
  GL.render
    texturedVertexShader
    texturedFragmentShader
    box
    { offset = Vec2.fromTuple (x + toFloat index * 4, y)
    , texture = texture
    , textureOffset = vec2 (toFloat number * 3) 50
    , textureSize = vec2 (toFloat (fst (GL.textureSize texture))) (toFloat (snd (GL.textureSize texture)))
    , size = vec2 3 4
    }
  |> (,) 0


renderLive : GL.Texture -> (Float, Float) -> (Int, GL.Renderable)
renderLive texture position =
  GL.render
    texturedVertexShader
    texturedFragmentShader
    box
    { offset = Vec2.fromTuple position
    , texture = texture
    , textureOffset = vec2 56 0
    , textureSize = vec2 (toFloat (fst (GL.textureSize texture))) (toFloat (snd (GL.textureSize texture)))
    , size = vec2 5 6
    }
  |> (,) 0


renderTitle : GL.Texture -> (Float, Float) -> (Int, GL.Renderable)
renderTitle texture position =
  GL.render
    texturedVertexShader
    texturedFragmentShader
    box
    { offset = Vec2.fromTuple position
    , texture = texture
    , textureOffset = vec2 0 15
    , textureSize = vec2 (toFloat (fst (GL.textureSize texture))) (toFloat (snd (GL.textureSize texture)))
    , size = vec2 58 24
    }
  |> (,) 0


renderPlay : GL.Texture -> (Float, Float) -> (Int, GL.Renderable)
renderPlay texture position =
  GL.render
    texturedVertexShader
    texturedFragmentShader
    box
    { offset = Vec2.fromTuple position
    , texture = texture
    , textureOffset = vec2 0 39
    , textureSize = vec2 (toFloat (fst (GL.textureSize texture))) (toFloat (snd (GL.textureSize texture)))
    , size = vec2 54 11
    }
  |> (,) 0


-- Shaders

texturedVertexShader : GL.Shader View.Common.Vertex UniformTextured Varying
texturedVertexShader = [glsl|

  precision mediump float;
  attribute vec2 position;
  uniform vec2 offset;
  uniform vec2 size;
  varying vec2 texturePos;

  void main () {
    vec2 roundOffset = vec2(floor(offset.x + 0.5), floor(offset.y + 0.5));
    vec2 clipSpace = (position * size + roundOffset) / 32.0 - 1.0;
    gl_Position = vec4(clipSpace.x, -clipSpace.y, 0, 1);
    texturePos = position * size;
  }

|]


texturedFragmentShader : GL.Shader {} UniformTextured Varying
texturedFragmentShader = [glsl|

  precision mediump float;
  uniform sampler2D texture;
  uniform vec2 textureSize;
  varying vec2 texturePos;
  uniform vec2 textureOffset;

  void main () {
    vec2 textureClipSpace = texturePos / textureSize - 1.0;
    vec2 offset = textureOffset / textureSize;
    vec4 temp = texture2D(texture, vec2(textureClipSpace.x + offset.x, -textureClipSpace.y - offset.y));
    float a = temp.a;
    gl_FragColor = vec4(temp.r * a, temp.g * a, temp.b * a, a);
  }

|]
