module View (view) where

import WebGL as GL
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Graphics.Element exposing (Element)
import Model exposing (Model)
import Model.Object as Object exposing (Object, Category(..))


type alias Vertex =
  { position : Vec2 }


type alias UniformTextured =
  { frame : Float
  , frameSize : Vec2
  , offset : Vec2
  , mirror : Float  -- 1 for right, -1 for left
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
    (x, y) = model.objects
      |> List.filter Object.isMogee
      |> List.head
      |> Maybe.map .position
      |> Maybe.withDefault (0, 0)

    offset = vec2 (x - 32 + 4) (y - 32 + 5)

    allScr = List.map (.offset >> \(x, y) -> (x / 64, y / 64)) model.screens
    maxX = List.maximum (List.map fst allScr) |> Maybe.withDefault 0
    minY = List.minimum (List.map snd allScr) |> Maybe.withDefault 0

    dot (x1, y1) =
      GL.render
        coloredVertexShader
        coloredFragmentShader
        box
        { offset = vec2 (63 - maxX - 1 + x1) (y1 - minY + 1)
        , color =
            if floor x1 == floor (x / 64) && floor y1 == floor (y / 64) then
              vec3 255 255 0
            else
              vec3 100 100 100
        , frameSize = vec2 1 1
        }

    bg =
      GL.render
        coloredVertexShader
        coloredFragmentShader
        box
        { offset = vec2 0 0
        , color = vec3 25 30 28
        , frameSize = vec2 64 64
        }
  in
    List.map dot allScr ++
    List.foldl (renderObject texture offset >> (::)) [bg] model.objects


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
        , frame = List.head mogee.frames |> Maybe.withDefault 0
        , mirror = if fst object.velocity < 0 then -1 else 1
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
    gl_FragColor = vec4(color / 255.0, 1);
  }

|]
