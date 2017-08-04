module Components.Components
    exposing
        ( Components
        , isDead
        , components
        , delete
        , mogeeOffset
        , addScreen
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


mogeeOffset : Components -> ( Float, Float )
mogeeOffset components =
    components.mogees
        |> Dict.keys
        |> List.head
        |> Maybe.andThen (\uid -> Dict.get uid components.transforms)
        |> Maybe.map .position
        -- this should never happen
        |> Maybe.withDefault ( 32, 32 )


foldl2 : (EntityId -> a -> b -> c -> c) -> c -> Dict EntityId a -> Dict EntityId b -> c
foldl2 fn initial component1 component2 =
    Dict.foldl
        (\uid a acc ->
            case Dict.get uid component2 of
                Just b ->
                    fn uid a b acc

                Nothing ->
                    acc
        )
        initial
        component1


foldl3 : (EntityId -> a -> b -> c -> d -> d) -> d -> Dict EntityId a -> Dict EntityId b -> Dict EntityId c -> d
foldl3 fn initial component1 component2 component3 =
    Dict.foldl
        (\uid a acc ->
            case ( Dict.get uid component2, Dict.get uid component3 ) of
                ( Just b, Just c ) ->
                    fn uid a b c acc

                _ ->
                    acc
        )
        initial
        component1


components : Components
components =
    { uid = 0
    , mogees = Dict.empty
    , screens = Dict.empty
    , transforms = Dict.empty
    , velocities = Dict.empty
    , walls = Dict.empty
    }
        |> addMogee ( 28, 27 )
        |> addScreen Left Right 0


delete : EntityId -> Components -> Components
delete uid components =
    { components
        | mogees = Dict.remove uid components.mogees
        , screens = Dict.remove uid components.screens
        , transforms = Dict.remove uid components.transforms
        , velocities = Dict.remove uid components.velocities
        , walls = Dict.remove uid components.walls
    }


addScreen : Direction -> Direction -> Int -> Components -> Components
addScreen from to number components =
    { components
        | uid = components.uid + 1
        , screens = Dict.insert components.uid (Screen.screen from to number) components.screens
        , transforms = Dict.insert components.uid { position = ( 0, 0 ), size = ( Screen.size, Screen.size ) } components.transforms
    }
        |> addWall ( 7, 2 ) ( 0, 11 )
        |> addWall ( 16, 2 ) ( 24, 11 )
        |> addWall ( 11, 2 ) ( 6, 27 )
        |> addWall ( 13, 2 ) ( 51, 27 )
        |> addWall ( 11, 2 ) ( 0, 43 )
        |> addWall ( 33, 2 ) ( 31, 43 )
        |> addWall ( 19, 2 ) ( 17, 58 )
        |> addWalls from to


addWalls : Direction -> Direction -> Components -> Components
addWalls from to components =
    List.foldl
        (\d ->
            if d == Direction.opposite from || d == to then
                identity
            else
                case d of
                    Left ->
                        addWall ( 1, Screen.size ) ( 0, 0 )

                    Right ->
                        addWall ( 1, Screen.size ) ( Screen.size - 1, 0 )

                    Top ->
                        addWall ( Screen.size, 1 ) ( 0, 0 )

                    Bottom ->
                        addWall ( Screen.size, 1 ) ( 0, Screen.size - 1 )
        )
        components
        [ Left, Right, Top, Bottom ]


addMogee : ( Float, Float ) -> Components -> Components
addMogee position components =
    { components
        | uid = components.uid + 1
        , mogees = Dict.insert components.uid Mogee.mogee components.mogees
        , transforms = Dict.insert components.uid { position = position, size = Mogee.size } components.transforms
        , velocities = Dict.insert components.uid { velocity = ( 0, 0 ) } components.velocities
    }


addWall : ( Float, Float ) -> ( Float, Float ) -> Components -> Components
addWall size position components =
    { components
        | uid = components.uid + 1
        , walls = Dict.insert components.uid Wall.wall components.walls
        , transforms = Dict.insert components.uid { position = position, size = size } components.transforms
    }
