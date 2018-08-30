module Systems.Mogee exposing (run)

import Components.Components as Components exposing (Components)
import Components.Mogee as Mogee exposing (Mogee)
import Components.Transform as Transform exposing (Transform)
import Components.Velocity as Velocity exposing (Velocity)
import Dict


gravity : Float
gravity =
    0.000225


friction : Float
friction =
    0.00225


jumpVelocity : Float
jumpVelocity =
    0.09


walkVelocity : Float
walkVelocity =
    0.045


moveY : Float -> Float -> List Transform -> Transform -> Velocity -> ( Transform, Velocity, Maybe String )
moveY dt dy wallsTransforms originalTransform originalVelocity =
    List.foldl
        (\wall ( transform, velocity, audio ) ->
            if Transform.collide transform wall then
                if transform.y - originalTransform.y < 0 then
                    -- Hit the top wall
                    ( { transform | y = wall.y + wall.height }
                    , { velocity | vy = 0 }
                    , if velocity.vy < -0.05 then
                        Just "wall"

                      else
                        audio
                    )

                else
                    -- Hit the bottom wall
                    ( { transform | y = wall.y - transform.height }
                    , { velocity
                        | vy =
                            if dy == 1 then
                                -jumpVelocity

                            else
                                0
                      }
                    , if dy == 1 then
                        Just "jump"

                      else if velocity.vy > 0.05 then
                        Just "wall"

                      else
                        Nothing
                    )

            else
                ( transform, velocity, audio )
        )
        ( { originalTransform | y = originalTransform.y + originalVelocity.vy * dt + gravity * dt * dt * 0.5 }
        , { originalVelocity | vy = originalVelocity.vy + gravity * dt }
        , Nothing
        )
        wallsTransforms


moveX : Float -> Float -> List Transform -> Transform -> Velocity -> ( Transform, Velocity )
moveX dt dx wallsTransforms originalTransform originalVelocity =
    let
        { vx } =
            originalVelocity

        newVelocity =
            if dx == 0 then
                if vx /= 0 then
                    (vx / abs vx) * max (abs vx - friction * dt) 0

                else
                    0

            else
                dx * walkVelocity

        deltaX =
            dt * (vx + newVelocity) * 0.5

        x =
            if dx == 0 && deltaX == 0 then
                -- Nudge the character on the pixel grid
                toFloat (round originalTransform.x)

            else
                originalTransform.x
    in
    List.foldl
        (\wall ( transform, velocity ) ->
            if Transform.collide transform wall then
                if transform.x - originalTransform.x < 0 then
                    {- Hit the left wall -}
                    ( { transform | x = wall.x + wall.width }
                    , { velocity | vx = 0 }
                    )

                else
                    {- Hit the right wall -}
                    ( { transform | x = wall.x - transform.width }
                    , { velocity | vx = 0 }
                    )

            else
                ( transform, velocity )
        )
        ( { originalTransform | x = x + deltaX }
        , { originalVelocity | vx = newVelocity }
        )
        wallsTransforms


{-| TODO: Split physics into its own system
-}
run : Float -> { x : Float, y : Float } -> Components -> ( Components, Maybe String )
run elapsed { x, y } components =
    let
        wallsTransforms =
            Components.foldl2 (\_ _ -> (::)) [] components.walls components.transforms

        screensTransforms =
            Components.foldl2 (\_ _ -> (::)) [] components.screens components.transforms
    in
    Components.foldl3
        (\uid mogee transform velocity ( components_, sound ) ->
            let
                ( newYTransform, newYVelocity, newSound ) =
                    moveY elapsed y wallsTransforms transform velocity

                ( newTransform, newVelocity ) =
                    moveX elapsed x wallsTransforms newYTransform newYVelocity

                newMogee =
                    if List.any (Transform.collide transform) screensTransforms then
                        Mogee.update elapsed x mogee

                    else
                        Mogee.die mogee
            in
            ( { components_
                | transforms = Dict.insert uid newTransform components_.transforms
                , velocities = Dict.insert uid newVelocity components_.velocities
                , mogees = Dict.insert uid newMogee components_.mogees
              }
            , if newSound == Nothing then
                sound

              else
                newSound
            )
        )
        ( components, Nothing )
        components.mogees
        components.transforms
        components.velocities
