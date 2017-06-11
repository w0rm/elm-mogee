module View.Screen exposing (render)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Model.Screen exposing (Screen, AnimationState(..))
import View.Common exposing (box, texturedFragmentShader)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture
import Model.Direction exposing (Direction(..))


move : List Int
move =
    [ 0, 1, 2, 3, 0, 4, 5, 6 ]


fadeIn : List Int
fadeIn =
    [ 12, 12, 7, 8, 9, 10, 11, 0 ]


fadeOut : List Int
fadeOut =
    List.reverse fadeIn


frameOffset : List Int -> Float -> Float
frameOffset list frame =
    list
        |> List.drop (truncate frame)
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
render texture position size { state, from, to, frame } =
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
                    , textureOffset = vec2 (64 + 10 * frameOffset fadeIn frame) 0
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
                        , textureOffset = vec2 (64 + 10 * frameOffset fadeOut frame) 0
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
                    , textureOffset = vec2 (64 + 10 * frameOffset move frame) 0
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
          texturePos = position * frameSize;
        }

    |]
