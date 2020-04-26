module View.Lives exposing
    ( renderLives
    , renderScore
    )

import View.Sprite as Sprite
import WebGL exposing (Entity)
import WebGL.Texture exposing (Texture)


renderLives : Texture -> ( Float, Float, Float ) -> Int -> List Entity
renderLives sprite ( x, y, z ) lives =
    List.map
        (\i ->
            Sprite.render
                (Sprite.sprite "life")
                sprite
                ( x + toFloat i * 6, y, z )
        )
        (List.range 0 (lives - 1))


digitsList : Int -> List Int
digitsList n =
    let
        nn =
            n // 10

        r =
            modBy 10 n
    in
    if nn == 0 && r == 0 then
        []

    else
        r :: digitsList nn


renderScore : Texture -> ( Float, Float, Float ) -> Int -> List Entity
renderScore sprite ( x, y, z ) value =
    let
        digits =
            digitsList value |> List.reverse

        position =
            ( x - toFloat (List.length digits) * 2, y, z )
    in
    if value == 0 then
        []

    else
        List.indexedMap (renderDigit sprite position) digits


renderDigit : Texture -> ( Float, Float, Float ) -> Int -> Int -> Entity
renderDigit sprite ( x, y, z ) index number =
    Sprite.render
        (Sprite.sprite ("score-" ++ String.fromInt number))
        sprite
        ( x + toFloat index * 4, y, z )
