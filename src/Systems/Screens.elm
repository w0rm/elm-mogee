module Systems.Screens exposing (screens, Screens, run)

import Random
import Dict
import Components.Direction as Direction exposing (Direction(..))
import Components.Components as Components exposing (Components)
import Components.Transform as Transform exposing (Transform)
import Components.Screen as Screen exposing (Screen, AnimationState(..))
import Components.Mogee as Mogee
import Time exposing (Time)


type alias Screens =
    { number : Int
    , direction : Direction
    , seed : Random.Seed
    }


screens : Screens
screens =
    { number = 0
    , direction = Right
    , seed = Random.initialSeed 0
    }


offsetScreens : Direction -> Components -> Components
offsetScreens direction components =
    { components
        | transforms =
            Dict.map
                (always (Transform.offset ( Screen.size, Screen.size ) direction))
                components.transforms
    }


run : Time -> Components -> Screens -> ( Components, Screens )
run elapsed components screens =
    let
        newComponents =
            update elapsed components

        { x, y } =
            Components.mogeeOffset newComponents

        screenX =
            x - 32 + Mogee.width / 2

        screenY =
            y - 32 + Mogee.height / 2

        ( direction, seed ) =
            Random.step (Direction.next screens.direction) screens.seed
    in
        if abs screenX < 64 && abs screenY < 64 then
            ( Components.addScreen
                screens.direction
                direction
                (screens.number + 1)
                (offsetScreens (Direction.opposite screens.direction) newComponents)
            , { screens
                | direction = direction
                , seed = seed
                , number = screens.number + 1
              }
            )
        else
            ( newComponents, screens )


shrink : Time -> Screen -> Transform -> Transform
shrink dt { to, state, velocity } transform =
    let
        { x, y, width, height } =
            transform

        newW =
            max 0 (width - dt * velocity)

        newH =
            max 0 (height - dt * velocity)
    in
        if state /= Moving || width == 0 || height == 0 then
            transform
        else
            case to of
                Left ->
                    { transform | width = newW }

                Right ->
                    { transform
                        | width = newW
                        , x = x - newW + width
                    }

                Top ->
                    { transform | height = newH }

                Bottom ->
                    { transform
                        | height = newH
                        , y = y - newH + height
                    }


activate : List Screen -> Screen -> Screen
activate screens screen =
    if List.all (\{ number } -> number /= screen.number - 1) screens then
        Screen.activate screen
    else
        screen


update : Time -> Components -> Components
update elapsed components =
    Components.foldl2
        (\uid screen transform components ->
            if transform.width == 0 || transform.height == 0 then
                -- Delete the screen
                Components.delete uid components
            else
                let
                    newScreen =
                        screen
                            |> Screen.update elapsed
                            |> activate (Dict.values components.screens)

                    newTransform =
                        shrink elapsed newScreen transform
                in
                    { components
                        | screens = Dict.insert uid newScreen components.screens
                        , transforms = Dict.insert uid newTransform components.transforms
                    }
        )
        components
        components.screens
        components.transforms
