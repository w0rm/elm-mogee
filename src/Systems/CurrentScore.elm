module Systems.CurrentScore exposing (CurrentScore, currentScore, run)

import Components.Components as Components exposing (Components)
import Components.Transform as Transform


type alias CurrentScore =
    Int


currentScore : CurrentScore
currentScore =
    0


run : Components -> CurrentScore -> CurrentScore
run components currentScore_ =
    let
        -- we assume there is only one Mogee
        collidesWithMogee =
            Components.foldl2
                (\_ _ transform _ -> Transform.collide transform)
                (always False)
                components.mogees
                components.transforms
    in
    -- find the max screen number that collides with Mogee
    Components.foldl2
        (\_ { number } transform score ->
            if collidesWithMogee transform then
                max score number

            else
                score
        )
        currentScore_
        components.screens
        components.transforms
