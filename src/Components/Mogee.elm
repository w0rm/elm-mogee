module Components.Mogee
    exposing
        ( Mogee
        , AnimationState(..)
        , mogee
        , update
        , die
        , width
        , height
        , isDead
        )

{-| The Mogee component determines the state of the main character
-}

import Time exposing (Time)


type AnimationState
    = Walking
    | Standing
    | Dead


type alias Mogee =
    { elapsed : Time
    , frames : List Float
    , state : AnimationState
    }


isDead : Mogee -> Bool
isDead { state } =
    state == Dead


width : Float
width =
    7


height : Float
height =
    10


walking : List Float
walking =
    [ 4, 5, 5, 6 ]


standing : List Float
standing =
    [ 2, 1, 0, 1 ]


mogee : Mogee
mogee =
    { elapsed = 0
    , frames = standing
    , state = Standing
    }


die : Mogee -> Mogee
die mogee =
    { mogee
        | state = Dead
        , frames = [ 7 ]
    }


update : Time -> Float -> Mogee -> Mogee
update dt directionX mogee =
    let
        newMogee =
            updateState directionX mogee

        timeout =
            if newMogee.state == Standing then
                1000
            else
                200

        newElapsed =
            newMogee.elapsed + dt
    in
        if newElapsed > timeout then
            { newMogee
                | elapsed = newElapsed - timeout
                , frames = rotate newMogee.frames
            }
        else
            { newMogee
                | elapsed = newElapsed
            }


updateState : Float -> Mogee -> Mogee
updateState directionX mogee =
    case mogee.state of
        Standing ->
            if directionX /= 0 then
                { mogee
                    | state = Walking
                    , frames = walking
                }
            else
                mogee

        Walking ->
            if directionX == 0 then
                { mogee
                    | state = Standing
                    , frames = standing
                }
            else
                mogee

        Dead ->
            mogee


rotate : List a -> List a
rotate list =
    case list of
        [] ->
            []

        el :: rest ->
            rest ++ [ el ]
