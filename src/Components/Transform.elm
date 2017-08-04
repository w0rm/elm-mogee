module Components.Transform exposing (Transform, offset, collide, invertScreen)

{-| The Transform component determines the position and
size of each entity in the scene.
-}

import Components.Direction as Direction exposing (Direction(..))
import Components.Screen as Screen


type alias Transform =
    { size : ( Float, Float )
    , position : ( Float, Float )
    }


offset : ( Float, Float ) -> Direction -> Transform -> Transform
offset distance direction object =
    { object | position = Direction.offset distance object.position direction }


{-| TODO: remove this, because it depends on the screen size (=64)
-}
invertScreen : Direction -> Transform -> Transform
invertScreen to { size, position } =
    let
        ( x, y ) =
            position

        ( w, h ) =
            size
    in
        case to of
            Left ->
                { size = ( Screen.size - w, h )
                , position = ( x + w, y )
                }

            Right ->
                { size = ( Screen.size - w, h )
                , position = ( x - (Screen.size - w), y )
                }

            Top ->
                { size = ( w, Screen.size - h )
                , position = ( x, y + h )
                }

            Bottom ->
                { size = ( w, Screen.size - h )
                , position = ( x, y - (Screen.size - h) )
                }


collide : Transform -> Transform -> Bool
collide o1 o2 =
    let
        ( x1, y1 ) =
            o1.position

        ( w1, h1 ) =
            o1.size

        ( x2, y2 ) =
            o2.position

        ( w2, h2 ) =
            o2.size
    in
        x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2
