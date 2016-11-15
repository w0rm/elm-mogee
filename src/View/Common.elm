module View.Common
    exposing
        ( Vertex
        , box
        , rectangle
        )

import WebGL as GL
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3)


type alias Vertex =
    { position : Vec2 }


type alias UniformColored =
    { offset : Vec3
    , size : Vec2
    , color : Vec3
    }


box : GL.Drawable Vertex
box =
    GL.Triangle
        [ ( Vertex (vec2 0 0), Vertex (vec2 1 1), Vertex (vec2 1 0) )
        , ( Vertex (vec2 0 0), Vertex (vec2 0 1), Vertex (vec2 1 1) )
        ]


rectangle : ( Float, Float ) -> ( Float, Float, Float ) -> ( Int, Int, Int ) -> GL.Renderable
rectangle size offset ( r, g, b ) =
    GL.render
        coloredVertexShader
        coloredFragmentShader
        box
        { offset = Vec3.fromTuple offset
        , color = Vec3.fromTuple ( toFloat r, toFloat g, toFloat b )
        , size = Vec2.fromTuple size
        }


coloredVertexShader : GL.Shader Vertex UniformColored {}
coloredVertexShader =
    [glsl|

  precision mediump float;
  attribute vec2 position;
  uniform vec3 offset;
  uniform vec2 size;

  void main () {
    vec2 pos = position * size + offset.xy;
    vec2 roundOffset = vec2(floor(pos.x + 0.5), floor(pos.y + 0.5));
    vec2 clipSpace = roundOffset / 32.0 - 1.0;
    gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z / 10.0, 1);
  }

|]


coloredFragmentShader : GL.Shader {} UniformColored {}
coloredFragmentShader =
    [glsl|

  precision mediump float;
  uniform vec3 color;

  void main () {
    gl_FragColor = vec4(color / 255.0, 1);
  }

|]
