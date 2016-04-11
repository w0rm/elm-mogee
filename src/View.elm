module View (view) where

import WebGL as GL
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Graphics.Element exposing (Element)
import Model exposing (Model)
import Object exposing (Object, Category(..))


type alias Vertex =
  { position : Vec2 }


type alias UniformTextured =
  { frame : Int
  , frameSize : Vec2
  , offset : Vec2
  , texture : GL.Texture
  , textureSize : Vec2
  }


type alias UniformColored =
  { offset : Vec2
  , frameSize : Vec2
  , color : Vec3
  }


type alias Varying =
  { texturePos : Vec2 }


box : GL.Drawable Vertex
box =
  GL.Triangle
    [ (Vertex (vec2 0 0), Vertex (vec2 1 1), Vertex (vec2 1 0))
    , (Vertex (vec2 0 0), Vertex (vec2 0 1), Vertex (vec2 1 1))
    ]


view : Maybe GL.Texture -> Int -> Model -> Element
view maybeTexture size model =
  GL.webglWithConfig
    [ GL.Enable GL.Blend
    , GL.BlendFunc (GL.One, GL.OneMinusSrcAlpha)
    ]
    (size, size)
    ( case maybeTexture of
        Nothing ->
          []
        Just texture ->
          render texture model
    )


render : GL.Texture -> Model -> List GL.Renderable
render texture model =
  let
    offset = vec2 0 0
  in
    List.foldl (renderObject texture offset >> (::)) [] model.objects


renderObject : GL.Texture -> Vec2 -> Object -> GL.Renderable
renderObject texture offset object =
  case object.category of
    WallCategory ->
      GL.render
        coloredVertexShader
        coloredFragmentShader
        box
        { offset = Vec2.sub (Vec2.fromTuple object.position) offset
        , color = vec3 0 0 0
        , frameSize = Vec2.fromTuple object.size
        }
    MogeeCategory mogee ->
      GL.render
        texturedVertexShader
        texturedFragmentShader
        box
        { offset = Vec2.sub (Vec2.fromTuple object.position) offset
        , texture = texture
        , frame = 1
        , textureSize = vec2 (toFloat (fst (GL.textureSize texture))) (toFloat (snd (GL.textureSize texture)))
        , frameSize = Vec2.fromTuple object.size
        }


-- Shaders

texturedVertexShader : GL.Shader Vertex UniformTextured Varying
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
  uniform int frame;
  varying vec2 texturePos;

  void main () {
    vec2 size = frameSize / textureSize;
    int frames = int(1.0 / size.x);
    vec2 frameOffset = size * vec2(float(frame - frame / frames * frames), -float(frame / frames));
    vec2 textureClipSpace = size * texturePos - 1.0;
    vec4 temp = texture2D(texture, vec2(textureClipSpace.x, -textureClipSpace.y) + frameOffset);
    float a = temp.a;
    gl_FragColor = vec4(temp.r * a, temp.g * a, temp.b * a, a);
  }

|]


coloredVertexShader : GL.Shader Vertex UniformColored {}
coloredVertexShader = [glsl|

  precision mediump float;
  attribute vec2 position;
  uniform vec2 offset;
  uniform vec2 frameSize;

  void main () {
    vec2 roundOffset = vec2(floor(offset.x + 0.5), floor(offset.y + 0.5));
    vec2 clipSpace = (position * frameSize + roundOffset) / 32.0 - 1.0;
    gl_Position = vec4(clipSpace.x, -clipSpace.y, 0, 1);
  }

|]


coloredFragmentShader : GL.Shader {} UniformColored {}
coloredFragmentShader = [glsl|

  precision mediump float;
  uniform vec3 color;

  void main () {
    gl_FragColor = vec4(color, 1);
  }

|]
