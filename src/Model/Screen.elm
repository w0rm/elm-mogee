module Model.Screen (Screen, Direction(..), screen, next, offset, walls) where

import Model.Object as Object exposing (Object)
import Random exposing (Generator)

size : Float
size = 64


borderSize : Float
borderSize = 1


type Direction = Left | Right | Top | Bottom


type alias Screen =
  { offset : (Float, Float) -- screen offset
  , direction : Direction -- direction to
  }


screen : (Float, Float) -> Direction -> Screen
screen = Screen


offset : (Float, Float) -> Screen -> Screen
offset (dx, dy) screen =
  { screen
  | offset = (fst screen.offset - dx, snd screen.offset - dy)
  }


getDirection : (Float, Float) -> (Float, Float) -> Maybe Direction
getDirection p1 (x2, y2) =
  if p1 == (x2 - size, y2) then
    Just Right
  else if p1 == (x2 + size, y2) then
    Just Left
  else if p1 == (x2, y2 - size) then
    Just Bottom
  else if p1 == (x2, y2 + size) then
    Just Top
  else
    Nothing


offsetScreen : (Float, Float) -> Direction -> (Float, Float)
offsetScreen (x, y) dir =
  case dir of
    Left -> (x - size, y)
    Right -> (x + size, y)
    Top -> (x, y - size)
    Bottom -> (x, y + size)


opposite : Direction -> Direction
opposite dir =
  case dir of
    Left -> Right
    Right -> Left
    Top -> Bottom
    Bottom -> Top


next : List Screen -> Generator Screen
next screens =
  let
    scr = Maybe.withDefault (screen (0, 0) Right) (List.head screens)
    oppositeDirection = opposite scr.direction
    nextOffset = offsetScreen scr.offset scr.direction
    possibleDirections = List.filter ((/=) (opposite scr.direction)) [Right, Top, Bottom]
    nextScreen maybeDirection =
      case maybeDirection of
        Just nextDirection ->
          screen nextOffset nextDirection
        Nothing ->
          screen nextOffset Right
  in
    Random.map nextScreen (pickRandom possibleDirections)


walls : Direction -> Screen -> List Object
walls from {offset, direction} =
  let
    (dx, dy) = offset
    corner (x, y) = Object.wall (borderSize, borderSize) (x + dx, y + dy)
    horizontal (x, y) = Object.wall (size - 2 * borderSize, borderSize) (x + dx, y + dy)
    vertical (x, y) = Object.wall (borderSize, size - 2 * borderSize) (x + dx, y + dy)
    oppositeDir = opposite from
  in
    Object.wall (7, 2) (0 + dx, 11 + dy) ::
    Object.wall (16, 2) (24 + dx, 11 + dy) ::

    Object.wall (11, 2) (6 + dx, 27 + dy) ::
    Object.wall (13, 2) (51 + dx, 27 + dy) ::

    Object.wall (11, 2) (0 + dx, 43 + dy) ::
    Object.wall (33, 2) (31 + dx, 43 + dy) ::

    Object.wall (19, 2) (17 + dx, 59 + dy) ::


    List.map
      corner
      [ (0, 0)
      , (size - borderSize, 0)
      , (size - borderSize, size - borderSize)
      , (0, size - borderSize)
      ]
    ++
    ( List.filter (\d -> d /= oppositeDir && d /= direction) [Left, Right, Top, Bottom] |>
      List.map (\d ->
        case d of
          Left -> vertical (0, borderSize)
          Right -> vertical (size - borderSize, borderSize)
          Top -> horizontal (borderSize, 0)
          Bottom -> horizontal (borderSize, size - borderSize)
      )
    )


{-| generate random element from the list -}
pickRandom : List a -> Random.Generator (Maybe a)
pickRandom list =
  Random.map
    (\index -> List.head (List.drop index list))
    (Random.int 0 (List.length list - 1))
