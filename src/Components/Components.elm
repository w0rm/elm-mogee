module Components.Components
    exposing
        ( Components
        , isDead
        , initial
        , delete
        , mogeeOffset
        , addScreen
        , foldl
        , foldl2
        , foldl3
        )

import Components.Mogee as Mogee exposing (Mogee)
import Components.Screen as Screen exposing (Screen)
import Components.Transform as Transform exposing (Transform)
import Components.Velocity as Velocity exposing (Velocity)
import Components.Wall as Wall exposing (Wall)
import Components.Direction as Direction exposing (Direction(..))
import Dict exposing (Dict)


type alias EntityId =
    Int


type alias Components =
    { uid : EntityId
    , mogees : Dict EntityId Mogee
    , screens : Dict EntityId Screen
    , transforms : Dict EntityId Transform
    , velocities : Dict EntityId Velocity
    , walls : Dict EntityId Wall
    }


isDead : Components -> Bool
isDead =
    .mogees >> Dict.values >> List.any Mogee.isDead


mogeeOffset : Components -> Transform
mogeeOffset { mogees, transforms } =
    Dict.keys mogees
        |> List.head
        |> Maybe.andThen (\uid -> Dict.get uid transforms)
        -- this should never happen
        |> Maybe.withDefault
            { x = 32
            , y = 32
            , width = Mogee.width
            , height = Mogee.height
            }


foldl : (EntityId -> a -> b -> b) -> b -> Dict EntityId a -> b
foldl =
    Dict.foldl


foldl2 : (EntityId -> a -> b -> c -> c) -> c -> Dict EntityId a -> Dict EntityId b -> c
foldl2 fn initial component1 component2 =
    Dict.foldl
        (\uid a ->
            Maybe.map (fn uid a)
                (Dict.get uid component2)
                |> Maybe.withDefault identity
        )
        initial
        component1


foldl3 : (EntityId -> a -> b -> c -> d -> d) -> d -> Dict EntityId a -> Dict EntityId b -> Dict EntityId c -> d
foldl3 fn initial component1 component2 component3 =
    Dict.foldl
        (\uid a ->
            Maybe.map2 (fn uid a)
                (Dict.get uid component2)
                (Dict.get uid component3)
                |> Maybe.withDefault identity
        )
        initial
        component1


initial : Components
initial =
    { uid = 0
    , mogees = Dict.empty
    , screens = Dict.empty
    , transforms = Dict.empty
    , velocities = Dict.empty
    , walls = Dict.empty
    }
        |> addMogee 28 27
        |> addScreen { x = 0, y = 0, width = 64, height = 64 } Left Right 0


delete : EntityId -> Components -> Components
delete uid components =
    { components
        | mogees = Dict.remove uid components.mogees
        , screens = Dict.remove uid components.screens
        , transforms = Dict.remove uid components.transforms
        , velocities = Dict.remove uid components.velocities
        , walls = Dict.remove uid components.walls
    }


addScreen : Transform -> Direction -> Direction -> Int -> Components -> Components
addScreen ({ x, y } as transform) from to number components =
    { components
        | uid = components.uid + 1
        , screens = Dict.insert components.uid (Screen.screen from to number) components.screens
        , transforms = Dict.insert components.uid transform components.transforms
    }
        |> addWall { width = 7, height = 2, x = x, y = y + 11 }
        |> addWall { width = 16, height = 2, x = x + 24, y = y + 11 }
        |> addWall { width = 11, height = 2, x = x + 6, y = y + 27 }
        |> addWall { width = 13, height = 2, x = x + 51, y = y + 27 }
        |> addWall { width = 11, height = 2, x = x + 0, y = y + 43 }
        |> addWall { width = 33, height = 2, x = x + 31, y = y + 43 }
        |> addWall { width = 19, height = 2, x = x + 17, y = y + 58 }
        |> addWalls x y from to


addWalls : Float -> Float -> Direction -> Direction -> Components -> Components
addWalls x y from to components =
    List.foldl
        (\d ->
            if d == Direction.opposite from || d == to then
                identity
            else
                case d of
                    Left ->
                        addWall { width = 1, height = Screen.size, x = x, y = y }

                    Right ->
                        addWall { width = 1, height = Screen.size, x = x + Screen.size - 1, y = y }

                    Top ->
                        addWall { width = Screen.size, height = 1, x = x, y = y }

                    Bottom ->
                        addWall { width = Screen.size, height = 1, x = x, y = y + Screen.size - 1 }
        )
        components
        [ Left, Right, Top, Bottom ]


addMogee : Float -> Float -> Components -> Components
addMogee x y components =
    { components
        | uid = components.uid + 1
        , mogees = Dict.insert components.uid Mogee.mogee components.mogees
        , transforms = Dict.insert components.uid { x = x, y = y, width = Mogee.width, height = Mogee.height } components.transforms
        , velocities = Dict.insert components.uid { vx = 0, vy = 0 } components.velocities
    }


addWall : Transform -> Components -> Components
addWall transform components =
    { components
        | uid = components.uid + 1
        , walls = Dict.insert components.uid Wall.wall components.walls
        , transforms = Dict.insert components.uid transform components.transforms
    }
