module Systems.CurrentScore exposing (run, CurrentScore, currentScore)

import Components.Components as Components exposing (Components)
import Components.Transform as Transform


type alias CurrentScore =
    Int


currentScore : CurrentScore
currentScore =
    0


run : Components -> CurrentScore -> CurrentScore
run components currentScore =
    let
        -- we assume there is only one Mogee
        collidesWithMogee =
            Components.foldl2
                (\_ _ transform _ -> Transform.collide transform)
                (always False)
                components.mogees
                components.transforms
    in
        Components.foldl2
            (\_ { number } transform score ->
                if collidesWithMogee transform then
                    max score number
                else
                    score
            )
            currentScore
            components.screens
            components.transforms
