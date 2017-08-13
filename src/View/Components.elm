module View.Components exposing (render)

import View.Common exposing (rectangle)
import View.Mogee as Mogee
import View.Wall as Wall
import View.Screen as Screen
import View.Color as Color
import Components.Components as Components exposing (Components)
import Components.Transform as Transform
import WebGL exposing (Texture, Entity)


offsetBy : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
offsetBy ( x1, y1 ) ( x2, y2 ) =
    ( x2 - x1
    , y2 - y1
    )


renderWalls : Texture -> ( Float, Float ) -> Components -> List Entity -> List Entity
renderWalls texture offset { walls, transforms } entities =
    Components.foldl2
        (\_ _ { position, size } ->
            (::) (Wall.render texture (offsetBy offset position) size)
        )
        entities
        walls
        transforms


renderMogee : Texture -> Float -> Components -> List Entity -> List Entity
renderMogee texture directionX { mogees } entities =
    Components.foldl
        (\_ mogee -> (::) (Mogee.render texture ( 28, 27 ) directionX mogee))
        entities
        mogees


renderScreens : Texture -> ( Float, Float ) -> Components -> List Entity -> List Entity
renderScreens texture offset { screens, transforms } entities =
    Components.foldl2
        (\_ screen transform ->
            let
                position =
                    offsetBy offset transform.position

                monster =
                    Transform.invertScreen screen.to transform

                monsterPosition =
                    offsetBy offset monster.position
            in
                (::) (rectangle transform.size ( Tuple.first position, Tuple.second position, 5 ) Color.darkGreen)
                    >> (::) (rectangle monster.size ( Tuple.first monsterPosition, Tuple.second monsterPosition, 2 ) Color.darkBlue)
                    >> Screen.render texture monsterPosition transform.size screen
        )
        entities
        screens
        transforms


render : Texture -> Float -> ( Float, Float ) -> Components -> List Entity -> List Entity
render texture directionX offset components =
    renderWalls texture offset components
        >> renderMogee texture directionX components
        >> renderScreens texture offset components
