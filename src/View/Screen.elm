module View.Screen exposing (render)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Model.Screen exposing (Screen, AnimationState(..))
import View.Common exposing (box)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture
import Model.Direction exposing (Direction(..))


move : List Int
move =
    [ 1, 2, 3, 0, 4, 5, 6, 0 ]


fadeIn : List Int
fadeIn =
    [ 12, 12, 7, 8, 9, 10, 11, 0 ]


fadeOut : List Int
fadeOut =
    List.reverse fadeIn


frame : List Int -> Float -> Float
frame list elapsed =
    list
        |> List.drop ((64 - round (elapsed / 2)) % 8)
        |> List.head
        |> Maybe.withDefault 0
        |> toFloat


movingTransform : Direction -> Mat4
movingTransform direction =
    case direction of
        Left ->
            Mat4.identity

        Right ->
            Mat4.makeTranslate (vec3 64 0 0)

        Top ->
            Mat4.makeRotate (-pi / 2) (vec3 0 0 1)

        Bottom ->
            Mat4.mul
                (Mat4.makeTranslate (vec3 64 64 0))
                (Mat4.makeRotate (pi / 2) (vec3 0 0 1))


fadeInTransform : Direction -> Direction -> Mat4
fadeInTransform from to =
    case ( from, to ) of
        ( Right, Bottom ) ->
            (Mat4.makeRotate (pi / 2) (vec3 0 0 1))
                |> Mat4.mul (Mat4.makeRotate pi (vec3 0 1 0))
                |> Mat4.mul (Mat4.makeTranslate (vec3 0 0 0))

        ( Bottom, Right ) ->
            Mat4.identity

        ( Right, Top ) ->
            (Mat4.makeRotate (-pi / 2) (vec3 0 0 1))

        ( Top, Right ) ->
            (Mat4.makeRotate pi (vec3 1 0 0))
                |> Mat4.mul (Mat4.makeTranslate (vec3 0 64 0))

        _ ->
            Mat4.makeTranslate (vec3 -64 -64 0)


fadeOutTransform : Direction -> Direction -> Mat4
fadeOutTransform from to =
    case ( from, to ) of
        ( Right, Bottom ) ->
            Mat4.makeTranslate (vec3 0 0 0)

        ( Bottom, Right ) ->
            (Mat4.makeRotate (pi / 2) (vec3 0 0 1))
                |> Mat4.mul (Mat4.makeRotate pi (vec3 0 1 0))

        ( Right, Top ) ->
            (Mat4.makeRotate pi (vec3 1 0 0))
                |> Mat4.mul (Mat4.makeTranslate (vec3 0 0 0))

        ( Top, Right ) ->
            Mat4.makeRotate (-pi / 2) (vec3 0 0 1)
                |> Mat4.mul (Mat4.makeTranslate (vec3 0 64 0))

        _ ->
            Mat4.makeTranslate (vec3 -64 -64 0)


screenOffset : ( Float, Float ) -> ( Float, Float ) -> Direction -> Vec3
screenOffset ( x, y ) ( w, h ) direction =
    case direction of
        Right ->
            Vec3.fromTuple ( x - w, y, 2 )

        Bottom ->
            Vec3.fromTuple ( x, y - h, 2 )

        _ ->
            Vec3.fromTuple ( x, y, 2 )


fadeOffset : ( Float, Float ) -> Vec3
fadeOffset ( x, y ) =
    Vec3.fromTuple ( x, y, 2 )


type alias UniformTextured =
    { offset : Vec3
    , transform : Mat4
    , texture : Texture
    , textureSize : Vec2
    , textureOffset : Vec2
    , frameSize : Vec2
    }


type alias Varying =
    { texturePos : Vec2 }


render : Texture -> ( Float, Float ) -> ( Float, Float ) -> Screen -> List Entity -> List Entity
render texture position size { state, from, to, elapsed } =
    case state of
        Initial ->
            identity

        Rotating ->
            (::)
                (WebGL.entity
                    texturedVertexShader
                    texturedFragmentShader
                    box
                    { offset = fadeOffset position
                    , texture = texture
                    , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
                    , textureOffset = vec2 (64 + 10 * frame fadeIn elapsed) 0
                    , frameSize = vec2 10 64
                    , transform = fadeInTransform from to
                    }
                )
                >> (::)
                    (WebGL.entity
                        texturedVertexShader
                        texturedFragmentShader
                        box
                        { offset = fadeOffset position
                        , texture = texture
                        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
                        , textureOffset = vec2 (64 + 10 * frame fadeOut elapsed) 0
                        , frameSize = vec2 10 64
                        , transform = fadeOutTransform from to
                        }
                    )

        Moving ->
            (::)
                (WebGL.entity
                    texturedVertexShader
                    texturedFragmentShader
                    box
                    { offset = screenOffset position size to
                    , texture = texture
                    , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
                    , textureOffset = vec2 (64 + 10 * frame move elapsed) 0
                    , frameSize = vec2 10 64
                    , transform = movingTransform to
                    }
                )



-- Shaders


texturedVertexShader : Shader View.Common.Vertex UniformTextured Varying
texturedVertexShader =
    [glsl|

        precision mediump float;
        attribute vec2 position;
        uniform vec3 offset;
        uniform vec2 frameSize;
        uniform mat4 transform;
        varying vec2 texturePos;

        void main () {
          vec2 roundOffset = vec2(floor(offset.x + 0.5), floor(offset.y + 0.5));
          vec2 clipSpace = vec2(transform * vec4(position * frameSize, 0, 1)) + roundOffset - 32.0;
          gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
          texturePos = position;
        }

    |]


texturedFragmentShader : Shader {} UniformTextured Varying
texturedFragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        uniform vec2 textureSize;
        uniform vec2 textureOffset;
        uniform vec2 frameSize;
        varying vec2 texturePos;

        void main () {
          vec2 textureClipSpace = texturePos * frameSize / textureSize - 1.0;
          vec2 offset = textureOffset / textureSize;
          gl_FragColor = texture2D(texture, vec2(textureClipSpace.x + offset.x, -textureClipSpace.y - offset.y));
          if (gl_FragColor.a == 0.0) discard;
        }

    |]
