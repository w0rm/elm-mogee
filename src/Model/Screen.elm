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
    , elapsed : Time
    , state : AnimationState
    }


screen : Direction -> Direction -> Screen
screen from to =
    { from = from
    , to = to
    , elapsed = 0
    , state = Initial
    }


update : Time -> ( Float, Float ) -> Screen -> Screen
update dt ( vx, vy ) screen =
    case screen.state of
        Initial ->
            screen

        _ ->
            if screen.elapsed - vx * dt <= 1 then
                { screen
                    | elapsed = 64
                    , state = Moving
                }
            else
                { screen
                    | elapsed = screen.elapsed - vx * dt
                }


activate : Screen -> Screen
activate screen =
    case screen.state of
        Initial ->
            if screen.to == screen.from then
                { screen | state = Moving, elapsed = 64 }
            else
                { screen | state = Rotating, elapsed = 16 }

        _ ->
            screen
