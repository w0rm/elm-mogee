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
    0.01


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
    { from =
        -- the first screen should not have left border
        if number == 0 then
            to
        else
            from
    , to = to
    , number = number
    , frame = 0
    , state = Initial
    , velocity = velocity + 0.001 * toFloat number
    }


update : Time -> Screen -> Screen
update dt screen =
    case screen.state of
        Initial ->
            screen

        _ ->
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
    case screen.state of
        Initial ->
            if screen.to == screen.from then
                { screen | state = Moving, frame = 0 }
            else
                { screen | state = Rotating, frame = 0 }

        _ ->
            screen
