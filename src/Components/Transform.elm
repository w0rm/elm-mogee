module Components.Transform exposing
    ( Transform
    , collide
    , offsetBy
    , offsetTo
    , roundCoordinates
    )

{-| The Transform component determines the position and
size of each entity in the scene.
-}

import Components.Direction as Direction exposing (Direction(..))


type alias Transform =
    { width : Float
    , height : Float
    , x : Float
    , y : Float
    }


offsetBy : ( Float, Float ) -> Transform -> Transform
offsetBy ( x, y ) transform =
    { transform
        | x = transform.x - x
        , y = transform.y - y
    }


roundFloat : Float -> Float
roundFloat =
    round >> toFloat


roundCoordinates : Transform -> Transform
roundCoordinates { x, y, width, height } =
    { x = roundFloat x
    , y = roundFloat y
    , width = roundFloat (width + x) - roundFloat x -- keep the right edge
    , height = roundFloat (height + y) - roundFloat y -- keep the bottom edge
    }


offsetTo : Float -> Direction -> Transform -> Transform
offsetTo delta direction transform =
    case direction of
        Left ->
            { transform | x = transform.x - delta }

        Right ->
            { transform | x = transform.x + delta }

        Top ->
            { transform | y = transform.y - delta }

        Bottom ->
            { transform | y = transform.y + delta }


collide : Transform -> Transform -> Bool
collide t1 t2 =
    (t1.x < t2.x + t2.width)
        && (t1.x + t1.width > t2.x)
        && (t1.y < t2.y + t2.height)
        && (t1.y + t1.height > t2.y)
