module Components.Transform exposing (Transform, offsetBy, offset, collide, opposite)

{-| The Transform component determines the position and
size of each entity in the scene.
-}

import Components.Direction as Direction exposing (Direction(..))
import Components.Screen as Screen


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


offset : ( Float, Float ) -> Direction -> Transform -> Transform
offset ( dx, dy ) direction transform =
    case direction of
        Left ->
            { transform | x = transform.x - dx }

        Right ->
            { transform | x = transform.x + dx }

        Top ->
            { transform | y = transform.y - dy }

        Bottom ->
            { transform | y = transform.y + dy }


opposite : Direction -> Transform -> Transform
opposite direction transform =
    case direction of
        Left ->
            { transform | x = transform.x + transform.width }

        Right ->
            { transform | x = transform.x - (Screen.size - transform.width) }

        Top ->
            { transform | y = transform.y + transform.height }

        Bottom ->
            { transform | y = transform.y - (Screen.size - transform.height) }


collide : Transform -> Transform -> Bool
collide t1 t2 =
    (t1.x < t2.x + t2.width)
        && (t1.x + t1.width > t2.x)
        && (t1.y < t2.y + t2.height)
        && (t1.y + t1.height > t2.y)
