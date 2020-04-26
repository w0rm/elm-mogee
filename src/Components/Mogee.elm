module Components.Mogee exposing
    ( AnimationState(..)
    , Mogee
    , die
    , height
    , isDead
    , mogee
    , update
    , width
    )

{-| The Mogee component determines the state of the main character
-}


type AnimationState
    = Walking
    | Standing
    | Dead


type alias Mogee =
    { elapsed : Float
    , frames : List Int
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


walking : List Int
walking =
    [ 4, 5, 5, 6 ]


standing : List Int
standing =
    [ 2, 1, 0, 1 ]


mogee : Mogee
mogee =
    { elapsed = 0
    , frames = standing
    , state = Standing
    }


die : Mogee -> Mogee
die mogee_ =
    { mogee_
        | state = Dead
        , frames = [ 7 ]
    }


update : Float -> Float -> Mogee -> Mogee
update dt directionX mogee_ =
    let
        newMogee =
            updateState directionX mogee_

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
updateState directionX mogee_ =
    case mogee_.state of
        Standing ->
            if directionX /= 0 then
                { mogee_
                    | state = Walking
                    , frames = walking
                }

            else
                mogee_

        Walking ->
            if directionX == 0 then
                { mogee_
                    | state = Standing
                    , frames = standing
                }

            else
                mogee_

        Dead ->
            mogee_


rotate : List a -> List a
rotate list =
    case list of
        [] ->
            []

        el :: rest ->
            rest ++ [ el ]
