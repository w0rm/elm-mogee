port module Ports exposing
    ( gamepad
    , play
    , sound
    , stop
    )

import Json.Decode exposing (Value)


{-| port for turning audio on/off
-}
port sound : Bool -> Cmd msg


{-| port for sending audio to play
-}
port play : String -> Cmd msg


{-| port for sending audio to stop
-}
port stop : String -> Cmd msg


{-| port for handling gamepad
-}
port gamepad : (Value -> msg) -> Sub msg
