module Components.Velocity exposing (Velocity)

{-| The Velocity component determines the velocity vector,
that is affected by gravity, inputs and collisions.
-}


type alias Velocity =
    { velocity : ( Float, Float )
    }
