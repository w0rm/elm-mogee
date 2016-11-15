module Model.Keys
    exposing
        ( Keys
        , keyChange
        , directions
        )

import Keyboard exposing (KeyCode)


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , space : Bool
    , enter : Bool
    }


keyChange : Bool -> KeyCode -> (Keys -> Keys)
keyChange on keyCode =
    case keyCode of
        13 ->
            \k -> { k | enter = on }

        32 ->
            \k -> { k | space = on }

        37 ->
            \k -> { k | left = on }

        39 ->
            \k -> { k | right = on }

        38 ->
            \k -> { k | up = on }

        40 ->
            \k -> { k | down = on }

        _ ->
            identity


directions : Keys -> { x : Int, y : Int }
directions { left, right, up, down } =
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
        { x = direction left right
        , y = direction down up
        }
