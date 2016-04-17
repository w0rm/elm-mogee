module View.Mogee (render) where

import Model.Mogee exposing (Mogee, size, AnimationState(..))
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import WebGL as GL
import View.Common exposing (box)


type alias UniformTextured =
  { frame : Float
  , frameSize : Vec2
  , offset : Vec2
  , mirror : Float  -- 1 for right, -1 for left
  , texture : GL.Texture
  , textureSize : Vec2
  }


type alias Varying =
  { texturePos : Vec2 }


render : GL.Texture -> (Float, Float) -> Mogee -> Float -> (Int, GL.Renderable)
render texture position mogee mirror =
  let
    layer = if mogee.state == Dead then 1 else 4
  in
    GL.render
      texturedVertexShader
      texturedFragmentShader
      box
      { offset = Vec2.fromTuple position
      , texture = texture
      , frame = List.head mogee.frames |> Maybe.withDefault 0
      , mirror = mirror
      , textureSize = vec2 (toFloat (fst (GL.textureSize texture))) (toFloat (snd (GL.textureSize texture)))
      , frameSize = Vec2.fromTuple size
      }
      |> (,) layer


-- Shaders

texturedVertexShader : GL.Shader View.Common.Vertex UniformTextured Varying
texturedVertexShader = [glsl|

  precision mediump float;
  attribute vec2 position;
  uniform vec2 offset;
  uniform vec2 frameSize;
  varying vec2 texturePos;

  void main () {
    vec2 roundOffset = vec2(floor(offset.x + 0.5), floor(offset.y + 0.5));
    vec2 clipSpace = (position * frameSize + roundOffset) / 32.0 - 1.0;
    gl_Position = vec4(clipSpace.x, -clipSpace.y, 0, 1);
    texturePos = position;
  }

|]


texturedFragmentShader : GL.Shader {} UniformTextured Varying
texturedFragmentShader = [glsl|

  precision mediump float;
  uniform sampler2D texture;
  uniform vec2 textureSize;
  uniform vec2 frameSize;
  uniform float frame;
  uniform float mirror;
  varying vec2 texturePos;

  void main () {
    vec2 size = frameSize / textureSize;
    vec2 frameOffset = size * vec2((1.0 - mirror) / 2.0 + frame, 0);
    vec2 textureClipSpace = size * vec2(texturePos.x * mirror, texturePos.y) - 1.0;
    vec4 temp = texture2D(texture, vec2(textureClipSpace.x, -textureClipSpace.y) + frameOffset);
    float a = temp.a;
    gl_FragColor = vec4(temp.r * a, temp.g * a, temp.b * a, a);
  }

|]
