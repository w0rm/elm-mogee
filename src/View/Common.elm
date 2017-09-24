module View.Common
    exposing
        ( Vertex
        , box
        , rectangle
        , writeMask
        , cropMask
        , texturedFragmentShader
        )

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Shader, Mesh, Entity, Texture)
import WebGL.Settings exposing (Setting)
import WebGL.Settings.StencilTest as StencilTest
import WebGL.Settings.DepthTest as DepthTest
import Components.Transform exposing (Transform)


type alias Vertex =
    { position : Vec2 }


type alias UniformColored =
    { offset : Vec3
    , size : Vec2
    , color : Vec3
    }


writeMask : Int -> Setting
writeMask ref =
    StencilTest.test
        { ref = ref
        , mask = 0xFF
        , test = StencilTest.always -- pass for each pixel
        , fail = StencilTest.keep -- noop
        , zfail = StencilTest.keep -- noop
        , zpass = StencilTest.replace -- write ref to the stencil buffer
        , writeMask = 0xFF -- enable all stencil bits for writing
        }


cropMask : Int -> Setting
cropMask ref =
    StencilTest.test
        { ref = ref
        , mask = 0xFF
        , test = StencilTest.equal -- pass when the stencil value is equal to ref = 1
        , fail = StencilTest.keep -- noop
        , zfail = StencilTest.keep -- noop
        , zpass = StencilTest.keep -- noop
        , writeMask = 0 -- disable writing to the stencil buffer
        }


box : Mesh Vertex
box =
    WebGL.triangles
        [ ( Vertex (vec2 0 0), Vertex (vec2 1 1), Vertex (vec2 1 0) )
        , ( Vertex (vec2 0 0), Vertex (vec2 0 1), Vertex (vec2 1 1) )
        ]


rectangle : Bool -> Transform -> Float -> Vec3 -> Entity
rectangle mask { x, y, width, height } l color =
    (if mask then
        WebGL.entityWith [ writeMask 1, DepthTest.default ]
     else
        WebGL.entity
    )
        coloredVertexShader
        coloredFragmentShader
        box
        { offset = vec3 x y l
        , color = color
        , size = vec2 width height
        }


coloredVertexShader : Shader Vertex UniformColored {}
coloredVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        uniform vec3 offset;
        uniform vec2 size;

        void main () {
          vec2 clipSpace = position * size + offset.xy - 32.0;
          gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
        }

    |]


coloredFragmentShader : Shader {} UniformColored {}
coloredFragmentShader =
    [glsl|

        precision mediump float;
        uniform vec3 color;

        void main () {
          gl_FragColor = vec4(color, 1);
        }

    |]


type alias UniformTextured a =
    { a
        | texture : Texture
        , textureSize : Vec2
        , textureOffset : Vec2
        , frameSize : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


texturedFragmentShader : Shader {} (UniformTextured a) Varying
texturedFragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        uniform vec2 textureSize;
        uniform vec2 textureOffset;
        uniform vec2 frameSize;
        varying vec2 texturePos;

        void main () {
          vec2 pos = vec2(
            float(int(texturePos.x) - int(texturePos.x) / int(frameSize.x) * int(frameSize.x)),
            float(int(texturePos.y) - int(texturePos.y) / int(frameSize.y) * int(frameSize.y))
          );
          vec2 offset = (pos + textureOffset) / textureSize;
          gl_FragColor = texture2D(texture, offset);
          if (gl_FragColor.a == 0.0) discard;
        }

    |]
