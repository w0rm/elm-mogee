module View.Components exposing (render)

import View.Common exposing (rectangle)
import View.Mogee as Mogee
import View.Wall as Wall
import View.Screen as Screen
import View.Color as Color
import Components.Components as Components exposing (Components)
import Components.Transform as Transform
import WebGL exposing (Texture, Entity)


renderWalls : Texture -> ( Float, Float ) -> Components -> List Entity -> List Entity
renderWalls texture offset { walls, transforms } entities =
    Components.foldl2
        (\_ _ transform ->
            (::) (Wall.render texture (Transform.offsetBy offset transform))
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
                screenTransform =
                    Transform.offsetBy offset transform

                monster =
                    transform
                        |> Transform.invertScreen screen.to
                        |> Transform.offsetBy offset
            in
                (::) (rectangle screenTransform 5 Color.darkGreen)
                    >> (::) (rectangle monster 2 Color.darkBlue)
                    >> Screen.render texture ( monster.x, monster.y ) ( transform.width, transform.height ) screen
        )
        entities
        screens
        transforms


render : Texture -> Float -> ( Float, Float ) -> Components -> List Entity -> List Entity
render texture directionX offset components =
    renderWalls texture offset components
        >> renderMogee texture directionX components
        >> renderScreens texture offset components