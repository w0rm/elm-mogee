module Components.Keys
    exposing
        ( Keys
        , keyChange
        , directions
        , initial
        , codes
        , pressed
        , down
        , anykey
        , animate
        )

import Keyboard exposing (KeyCode)
import Dict exposing (Dict)
import Time exposing (Time)


type alias Keys =
    Dict KeyCode Time


initial : Keys
initial =
    Dict.empty


codes :
    { down : KeyCode
    , enter : KeyCode
    , left : KeyCode
    , right : KeyCode
    , space : KeyCode
    , up : KeyCode
    , escape : KeyCode
    }
codes =
    { enter = 13
    , space = 32
    , escape = 27
    , left = 37
    , right = 39
    , up = 38
    , down = 40
    }


keyChange : Bool -> KeyCode -> Keys -> Keys
keyChange on code =
    if on then
        Dict.insert code 0
    else
        Dict.remove code


animate : Time -> Keys -> Keys
animate elapsed =
    Dict.map (\_ -> (+) elapsed)


pressed : KeyCode -> Keys -> Bool
pressed code keys =
    Dict.get code keys == Just 0


down : KeyCode -> Keys -> Bool
down =
    Dict.member


anykey : Keys -> Bool
anykey =
    Dict.values >> List.member 0


directions : Keys -> { x : Float, y : Float }
directions keys =
    let
        direction a b =
            case ( a, b ) of
                ( True, False ) ->
                    -1

                ( False, True ) ->
                    1

                _ ->
                    0
    in
        { x = direction (down codes.left keys) (down codes.right keys)
        , y = direction (down codes.down keys) (down codes.up keys)
        }
