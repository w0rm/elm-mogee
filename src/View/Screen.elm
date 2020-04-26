module View.Screen exposing (render)

import Components.Direction exposing (Direction(..))
import Components.Screen exposing (AnimationState(..), Screen)
import Components.Transform exposing (Transform)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (vec3)
import View.Sprite as Sprite exposing (Sprite)
import WebGL exposing (Entity)
import WebGL.Texture exposing (Texture)


move : List Int
move =
    [ 0, 1, 2, 3, 0, 4, 5, 6 ]


fadeIn : List Int
fadeIn =
    [ 12, 12, 7, 8, 9, 10, 11, 0 ]


fadeOut : List Int
fadeOut =
    List.reverse fadeIn


frameSprite : List Int -> Float -> Sprite
frameSprite list frame =
    list
        |> List.drop (truncate frame)
        |> List.head
        |> Maybe.map (\i -> "monster-" ++ String.fromInt i)
        |> Maybe.withDefault "monster-0"
        |> Sprite.sprite


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


moveOffset : Transform -> Direction -> ( Float, Float, Float )
moveOffset { x, y, width, height } direction =
    case direction of
        Left ->
            ( x + width, y, 2 )

        Top ->
            ( x, y + height, 2 )

        _ ->
            ( x, y, 2 )


fadeOffset : Transform -> ( Float, Float, Float )
fadeOffset { x, y } =
    ( x, y, 2 )


render : Texture -> Transform -> Screen -> List Entity -> List Entity
render sprite transform { state, from, to, frame } =
    case state of
        Initial ->
            identity

        Rotating ->
            (::)
                (Sprite.renderTransformed
                    (frameSprite fadeIn frame)
                    sprite
                    (fadeTransform from to)
                    (fadeOffset transform)
                )
                >> (::)
                    (Sprite.renderTransformed
                        (frameSprite fadeOut frame)
                        sprite
                        (fadeTransform to from)
                        (fadeOffset transform)
                    )

        Moving ->
            (::)
                (Sprite.renderTransformed
                    (frameSprite move frame)
                    sprite
                    (movingTransform to)
                    (moveOffset transform to)
                )
