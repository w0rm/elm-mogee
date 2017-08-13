module Systems.Systems exposing (Systems, systems, run)

import Systems.Screens as Screens exposing (Screens)
import Systems.CurrentScore as CurrentScore exposing (CurrentScore)
import Systems.Mogee as Mogee
import Systems.Walls as Walls
import Components.Components as Components exposing (Components)
import Time exposing (Time)


type alias Systems =
    { screens : Screens
    , currentScore : CurrentScore
    }


systems : Systems
systems =
    { screens = Screens.screens
    , currentScore = CurrentScore.currentScore
    }


run : Time -> { x : Float, y : Float } -> Components -> Systems -> ( Components, Systems )
run elapsed keys components { screens, currentScore } =
    let
        ( components1, newScreens ) =
            Screens.run elapsed components screens

        components2 =
            components1
                |> Mogee.run elapsed keys
                |> Walls.run
    in
        ( components2
        , { screens = newScreens
          , currentScore = CurrentScore.run components2 currentScore
          }
        )
