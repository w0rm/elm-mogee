module Components.Gamepad exposing
    ( Gamepad
    , fromJson
    )

import Array exposing (Array)
import Json.Decode as D exposing (Decoder)


type alias Gamepad =
    { a : Bool
    , left : Bool
    , right : Bool
    }


type alias JsGamepad =
    { buttons : Array Button
    , axes : Array Float
    }


type alias Button =
    { value : Float
    , pressed : Bool
    }


fromJson : D.Value -> Gamepad
fromJson json =
    D.decodeValue decoder json
        |> Result.withDefault (Gamepad False False False)


decoder : Decoder Gamepad
decoder =
    D.map2 JsGamepad
        (D.field "buttons" (D.array buttonDecoder))
        (D.field "axes" (D.array D.float))
        |> D.map toGamepad


buttonDecoder : Decoder Button
buttonDecoder =
    D.map2 Button
        (D.field "value" D.float)
        (D.field "pressed" D.bool)


toGamepad : JsGamepad -> Gamepad
toGamepad { buttons, axes } =
    Gamepad
        (Array.get 0 buttons
            |> Maybe.map .pressed
            |> Maybe.withDefault False
        )
        (Array.get 0 axes
            |> Maybe.map (\float -> float < -0.25)
            |> Maybe.withDefault False
        )
        (Array.get 0 axes
            |> Maybe.map (\float -> float > 0.25)
            |> Maybe.withDefault False
        )
