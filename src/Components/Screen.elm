module Components.Screen
    exposing
        ( AnimationState(..)
        , Screen
        , screen
        , activate
        , update
        , size
        )

{-| The Screen component determines collapsing screens,
that the map is made of.
-}

import Components.Direction as Direction exposing (Direction)
import Time exposing (Time)


size : Float
size =
    64


velocity : Float
velocity =
    0.015


velocityIncrement : Float
velocityIncrement =
    0.0015


type AnimationState
    = Initial
    | Rotating
    | Moving


type alias Screen =
    { from : Direction
    , to : Direction
    , number : Int
    , frame : Time
    , state : AnimationState
    , velocity : Float
    }


screen : Direction -> Direction -> Int -> Screen
screen from to number =
    { from = from
    , to = to
    , number = number
    , frame = 0
    , state = Initial
    , velocity = velocity + velocityIncrement * toFloat number
    }


update : Time -> Screen -> Screen
update dt screen =
    if screen.state == Initial then
        screen
    else
        let
            frame =
                screen.frame + screen.velocity * dt / 2
        in
            if frame >= 8 then
                { screen | frame = 8 - frame, state = Moving }
            else
                { screen | frame = frame }


activate : Screen -> Screen
activate screen =
    if screen.state == Initial then
        if screen.to == screen.from then
            { screen | state = Moving, frame = 0 }
        else
            { screen | state = Rotating, frame = 0 }
    else
        screen
