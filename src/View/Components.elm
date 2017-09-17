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


renderMogee : Texture -> Texture -> ( Float, Float ) -> Float -> Components -> List Entity -> List Entity
renderMogee texture sprite offset directionX { mogees } entities =
    Components.foldl
        (\_ -> Mogee.render texture sprite ( 28, 27 ) offset directionX)
        entities
        mogees


renderScreens : Texture -> ( Float, Float ) -> Components -> List Entity -> List Entity
renderScreens texture offset { screens, transforms } entities =
    Components.foldl2
        (\_ screen transform ->
            let
                screenTransform =
                    Transform.offsetBy offset transform
            in
                (::) (rectangle True screenTransform 6 Color.darkGreen)
                    >> Screen.render texture screenTransform screen
        )
        entities
        screens
        transforms


render : Texture -> Texture -> Float -> ( Float, Float ) -> Components -> List Entity -> List Entity
render texture sprite directionX offset components =
    -- the order here matters, screens must be drawn first
    renderWalls texture offset components
        >> renderMogee texture sprite offset directionX components
        >> renderScreens texture offset components
