module View.Screen exposing (render)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Components.Screen exposing (Screen, AnimationState(..))
import View.Common exposing (box, texturedFragmentShader, writeMask)
import WebGL exposing (Texture, Shader, Mesh, Entity)
import WebGL.Texture as Texture
import Components.Transform exposing (Transform)
import Components.Direction exposing (Direction(..))
import WebGL.Settings.DepthTest as DepthTest


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
        Top ->
            Mat4.makeRotate (-pi / 2) (vec3 0 0 1)

        Bottom ->
            Mat4.makeRotate (pi / 2) (vec3 0 0 1)
                |> Mat4.mul (Mat4.makeTranslate (vec3 64 0 0))

        _ ->
            Mat4.identity


fadeTransform : Direction -> Direction -> Mat4
fadeTransform from to =
    case ( from, to ) of
        ( Bottom, Right ) ->
            Mat4.identity

        ( Right, Bottom ) ->
            Mat4.makeRotate (pi / 2) (vec3 0 0 1)
                |> Mat4.mul (Mat4.makeRotate pi (vec3 0 1 0))

        ( Right, Top ) ->
            Mat4.makeRotate (-pi / 2) (vec3 0 0 1)
                |> Mat4.mul (Mat4.makeTranslate (vec3 0 64 0))

        ( Top, Right ) ->
            Mat4.makeRotate pi (vec3 1 0 0)
                |> Mat4.mul (Mat4.makeTranslate (vec3 0 64 0))

        _ ->
            -- hide the monster
            Mat4.makeTranslate (vec3 -64 -64 0)


moveOffset : Transform -> Direction -> Vec3
moveOffset { x, y, width, height } direction =
    case direction of
        Left ->
            Vec3.fromTuple ( toFloat (round (x + width)), toFloat (round y), 2 )

        Top ->
            Vec3.fromTuple ( toFloat (round x), toFloat (round (y + height)), 2 )

        _ ->
            Vec3.fromTuple ( toFloat (round x), toFloat (round y), 2 )


fadeOffset : Transform -> Vec3
fadeOffset { x, y } =
    Vec3.fromTuple ( toFloat (round x), toFloat (round y), 2 )


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


render : Texture -> Transform -> Screen -> List Entity -> List Entity
render texture transform { state, from, to, frame } =
    case state of
        Initial ->
            identity

        Rotating ->
            (::)
                (WebGL.entityWith
                    [ DepthTest.default, writeMask 0 ]
                    texturedVertexShader
                    texturedFragmentShader
                    box
                    { offset = fadeOffset transform
                    , texture = texture
                    , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
                    , textureOffset = vec2 (64 + 10 * frameOffset fadeIn frame) 0
                    , frameSize = vec2 10 64
                    , transform = fadeTransform from to
                    }
                )
                >> (::)
                    (WebGL.entityWith
                        [ DepthTest.default, writeMask 0 ]
                        texturedVertexShader
                        texturedFragmentShader
                        box
                        { offset = fadeOffset transform
                        , texture = texture
                        , textureSize = vec2 (toFloat (Tuple.first (Texture.size texture))) (toFloat (Tuple.second (Texture.size texture)))
                        , textureOffset = vec2 (64 + 10 * frameOffset fadeOut frame) 0
                        , frameSize = vec2 10 64
                        , transform = fadeTransform to from
                        }
                    )

        Moving ->
            (::)
                (WebGL.entityWith
                    [ DepthTest.default, writeMask 0 ]
                    texturedVertexShader
                    texturedFragmentShader
                    box
                    { offset = moveOffset transform to
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
          vec2 clipSpace = vec2(transform * vec4(position * frameSize, 0, 1)) + offset.xy - 32.0;
          gl_Position = vec4(clipSpace.x, -clipSpace.y, offset.z, 32.0);
          texturePos = position * frameSize;
        }

    |]
