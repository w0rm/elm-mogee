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
    , elapsed = 64
    , state = Initial
    }


update : Time -> ( Float, Float ) -> Screen -> Screen
update dt ( vx, vy ) screen =
    case screen.state of
        Initial ->
            screen

        _ ->
            if screen.elapsed - vx * dt <= 0 then
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
                { screen | state = Moving }
            else
                { screen | state = Rotating }

        _ ->
            screen


move : List Float
move =
    [ 1, 2, 3, 0, 4, 5, 6, 0 ]


fadeIn : List Float
fadeIn =
    [ 7, 8, 9, 10, 11, 12, 18, 18 ]


fadeOut : List Float
fadeOut =
    [ 18, 18, 13, 14, 15, 16, 17, 0 ]
