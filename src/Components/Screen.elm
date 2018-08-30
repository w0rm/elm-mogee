module Components.Screen exposing
    ( AnimationState(..)
    , Screen
    , activate
    , screen
    , size
    , update
    )

{-| The Screen component determines collapsing screens,
that the map is made of.
-}

import Components.Direction as Direction exposing (Direction)


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
    , frame : Float
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


update : Float -> Screen -> Screen
update dt scr =
    if scr.state == Initial then
        scr

    else
        let
            frame =
                scr.frame + scr.velocity * dt / 2
        in
        if frame >= 8 then
            { scr | frame = 8 - frame, state = Moving }

        else
            { scr | frame = frame }


activate : Screen -> Screen
activate scr =
    if scr.state == Initial then
        if scr.to == scr.from then
            { scr | state = Moving, frame = 0 }

        else
            { scr | state = Rotating, frame = 0 }

    else
        scr
