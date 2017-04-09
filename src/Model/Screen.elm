module Model.Screen
    exposing
        ( AnimationState(..)
        , Screen
        , screen
        , activate
        , update
        )

import Model.Direction as Direction exposing (Direction)
import Time exposing (Time)


type AnimationState
    = Initial
    | Rotating
    | Moving


type alias Screen =
    { from : Direction
    , to : Direction
    , frame : Time
    , state : AnimationState
    }


screen : Direction -> Direction -> Screen
screen from to =
    { from = from
    , to = to
    , frame = 0
    , state = Initial
    }


update : Time -> ( Float, Float ) -> Screen -> Screen
update dt ( vx, vy ) screen =
    case screen.state of
        Initial ->
            screen

        _ ->
            let
                frame =
                    screen.frame + vx * dt / 2
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
