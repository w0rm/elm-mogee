module Systems.Walls exposing (run)

import Components.Components as Components exposing (Components)
import Components.Transform as Transform


{-| If a wall doesn't collide with any screen, it should be removed
-}
run : Components -> Components
run components =
    let
        screensTransforms =
            Components.foldl2 (\_ _ -> (::)) [] components.screens components.transforms
    in
    Components.foldl2
        (\uid _ transform components_ ->
            if List.any (Transform.collide transform) screensTransforms then
                components_

            else
                Components.delete uid components_
        )
        components
        components.walls
        components.transforms
