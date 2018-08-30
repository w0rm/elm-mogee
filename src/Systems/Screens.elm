module Systems.Screens exposing (Screens, run, screens)

import Components.Components as Components exposing (Components)
import Components.Direction as Direction exposing (Direction(..))
import Components.Mogee as Mogee
import Components.Screen as Screen exposing (AnimationState(..), Screen)
import Components.Transform as Transform exposing (Transform)
import Dict
import Random


type alias Screens =
    { number : Int -- a number of the last screen
    , transform : Transform -- a position of the last screen
    , direction : Direction -- a direction of the last screen
    , seed : Random.Seed
    }


screens : Screens
screens =
    { number = 0
    , transform = { x = 0, y = 0, width = Screen.size, height = Screen.size }
    , direction = Right
    , seed = Random.initialSeed 0
    }


run : Float -> Components -> Screens -> ( Components, Screens )
run elapsed components screens_ =
    let
        newComponents =
            update elapsed components

        { x, y } =
            Components.mogeeOffset newComponents

        screenX =
            x + Mogee.width / 2 - Screen.size / 2 - screens_.transform.x

        screenY =
            y + Mogee.height / 2 - Screen.size / 2 - screens_.transform.y

        newTransform =
            Transform.offsetTo Screen.size
                screens_.direction
                screens_.transform

        ( newDirection, newSeed ) =
            Random.step (Direction.next screens_.direction) screens_.seed

        newNumber =
            screens_.number + 1
    in
    if abs screenX < Screen.size && abs screenY < Screen.size then
        ( Components.addScreen
            newTransform
            screens_.direction
            newDirection
            newNumber
            newComponents
        , { screens_
            | direction = newDirection
            , seed = newSeed
            , number = newNumber
            , transform = newTransform
          }
        )

    else
        ( newComponents, screens_ )


shrink : Float -> Screen -> Transform -> Transform
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
activate screens_ screen =
    if List.all (\{ number } -> number /= screen.number - 1) screens_ then
        Screen.activate screen

    else
        screen


update : Float -> Components -> Components
update elapsed components =
    Components.foldl2
        (\uid screen transform components_ ->
            if transform.width == 0 || transform.height == 0 then
                -- Delete the screen
                Components.delete uid components_

            else
                let
                    newScreen =
                        screen
                            |> Screen.update elapsed
                            |> activate (Dict.values components_.screens)

                    newTransform =
                        shrink elapsed newScreen transform
                in
                { components_
                    | screens = Dict.insert uid newScreen components_.screens
                    , transforms = Dict.insert uid newTransform components_.transforms
                }
        )
        components
        components.screens
        components.transforms
