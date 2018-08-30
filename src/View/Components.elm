module View.Components exposing (render)

import Components.Components as Components exposing (Components)
import Components.Transform as Transform exposing (Transform)
import View.Color as Color
import View.Common exposing (rectangle)
import View.Mogee as Mogee
import View.Screen as Screen
import View.Wall as Wall
import WebGL exposing (Entity)
import WebGL.Texture exposing (Texture)


viewport : Transform
viewport =
    { x = 0
    , y = 0
    , width = 64
    , height = 64
    }


renderWalls : Texture -> Components -> List Entity -> List Entity
renderWalls texture { walls, transforms } entities =
    {- Query by transforms first and then inner join with walls,
       because there are less transforms than walls
    -}
    Components.foldl2
        (\_ transform _ -> Wall.render texture transform |> (::))
        entities
        transforms
        walls


renderMogee : Texture -> Float -> Components -> List Entity -> List Entity
renderMogee texture directionX { mogees, transforms } entities =
    Components.foldl2
        (\_ -> Mogee.render texture directionX)
        entities
        mogees
        transforms


renderScreens : Texture -> Components -> List Entity -> List Entity
renderScreens texture { screens, transforms } entities =
    Components.foldl2
        (\_ screen transform ->
            (::) (rectangle True transform 6 Color.darkGreen)
                >> Screen.render texture transform screen
        )
        entities
        screens
        transforms


render : Texture -> Texture -> Float -> ( Float, Float ) -> Components -> List Entity -> List Entity
render texture sprite directionX cameraOffset components =
    let
        newComponents =
            visibleComponents cameraOffset components
    in
    {- the order matters, screens must be drawn first,
       because they set the stencil buffer
    -}
    renderWalls texture newComponents
        >> renderMogee texture directionX newComponents
        >> Mogee.renderBg sprite cameraOffset
        >> renderScreens texture newComponents


{-| Offsets all transform components and filters the ones
that collide with the viewport
-}
visibleComponents : ( Float, Float ) -> Components -> Components
visibleComponents cameraOffset components =
    { components
        | transforms =
            components.transforms
                |> Components.map (\_ -> Transform.offsetBy cameraOffset >> Transform.roundCoordinates)
                |> Components.filter (\_ -> Transform.collide viewport)
    }
