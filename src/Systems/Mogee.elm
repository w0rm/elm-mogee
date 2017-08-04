module Systems.Mogee exposing (run)

import Components.Components as Components exposing (Components)
import Components.Transform as Transform exposing (Transform)
import Components.Velocity as Velocity exposing (Velocity)
import Components.Mogee as Mogee exposing (Mogee)
import Time exposing (Time)
import Dict


gravity : Float
gravity =
    0.0001


friction : Float
friction =
    0.001


jumpVelocity : Float
jumpVelocity =
    0.06


walkVelocity : Float
walkVelocity =
    0.03


moveY : Time -> Float -> List Transform -> Transform -> Velocity -> ( Transform, Velocity )
moveY dt dy wallsTransforms transform velocity =
    let
        ( vx, vy ) =
            velocity.velocity

        ( x, y ) =
            transform.position

        newVelocity =
            vy + gravity * dt

        deltaY =
            dt * (vy + newVelocity) * 0.5
    in
        List.foldl
            (\({ position, size } as wall) ( transform, velocity ) ->
                if Transform.collide transform wall then
                    if deltaY < 0 then
                        -- Hit the top wall
                        ( { transform | position = ( x, Tuple.second position + Tuple.second size ) }
                        , { velocity | velocity = ( vx, 0 ) }
                        )
                    else
                        -- Hit the bottom wall
                        ( { transform | position = ( x, Tuple.second position - Tuple.second transform.size ) }
                        , { velocity
                            | velocity =
                                ( vx
                                , if dy == 1 then
                                    -jumpVelocity
                                  else
                                    0
                                )
                          }
                        )
                else
                    ( transform, velocity )
            )
            ( { transform | position = ( x, y + deltaY ) }
            , { velocity | velocity = ( vx, newVelocity ) }
            )
            wallsTransforms


moveX : Time -> Float -> List Transform -> Transform -> Velocity -> ( Transform, Velocity )
moveX dt dx wallsTransforms transform velocity =
    let
        ( vx, vy ) =
            velocity.velocity

        newVelocity =
            if dx == 0 then
                if vx /= 0 then
                    (vx / abs vx) * (max (abs vx - friction * dt) 0)
                else
                    0
            else
                dx * walkVelocity

        deltaX =
            dt * (vx + newVelocity) * 0.5

        ( x, y ) =
            if dx == 0 && deltaX == 0 then
                -- Nudge the character on the pixel grid
                ( toFloat (round (Tuple.first transform.position)), Tuple.second transform.position )
            else
                transform.position
    in
        List.foldl
            (\({ position, size } as wall) ( transform, velocity ) ->
                if Transform.collide transform wall then
                    if deltaX < 0 then
                        {- Hit the left wall -}
                        ( { transform | position = ( Tuple.first position + Tuple.first size, y ) }
                        , { velocity | velocity = ( 0, vy ) }
                        )
                    else
                        {- Hit the right wall -}
                        ( { transform | position = ( Tuple.first position - Tuple.first transform.size, y ) }
                        , { velocity | velocity = ( 0, vy ) }
                        )
                else
                    ( transform, velocity )
            )
            ( { transform | position = ( x + deltaX, y ) }
            , { velocity | velocity = ( newVelocity, vy ) }
            )
            wallsTransforms


{-| TODO: Split physics into its own system
-}
run : Time -> { x : Float, y : Float } -> Components -> Components
run elapsed { x, y } components =
    let
        wallsTransforms =
            Components.foldl2 (\_ _ -> (::)) [] components.walls components.transforms

        screensTransforms =
            Components.foldl2 (\_ _ -> (::)) [] components.screens components.transforms
    in
        Components.foldl3
            (\uid mogee transform velocity components ->
                let
                    ( newTransform, newVelocity ) =
                        moveY elapsed y wallsTransforms transform velocity
                            |> uncurry (moveX elapsed x wallsTransforms)

                    newMogee =
                        if List.any (Transform.collide transform) screensTransforms then
                            Mogee.update elapsed x mogee
                        else
                            Mogee.die mogee
                in
                    { components
                        | transforms = Dict.insert uid newTransform components.transforms
                        , velocities = Dict.insert uid newVelocity components.velocities
                        , mogees = Dict.insert uid newMogee components.mogees
                    }
            )
            components
            components.mogees
            components.transforms
            components.velocities
