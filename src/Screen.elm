module Screen (Screen, screen, walls) where

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Direction exposing (Direction(..))
import Object exposing (Object)


size : Float
size = 64


borderSize : Float
borderSize = 2


type alias Screen =
  { offset : Vec2 -- screen offset
  , direction : Direction -- direction to
  }


screen : Vec2 -> Direction -> Screen
screen = Screen


walls : Direction -> Screen -> List Object
walls from {offset, direction} =
  let
    corner = Vec2.add offset >> Object.wall (vec2 borderSize borderSize)
    horizontal = Vec2.add offset >> Object.wall (vec2 (size - 2 * borderSize) borderSize)
    vertical = Vec2.add offset >> Object.wall (vec2 borderSize (size - 2 * borderSize))
  in
    List.map
      corner
      [ vec2 0 0
      , vec2 (size - borderSize) 0
      , vec2 (size - borderSize) (size - borderSize)
      , vec2 0 (size - borderSize)
      ]
    ++
    ( List.filter (\d -> d /= from && d /= direction) [Left, Top, Right, Bottom] |>
      List.map (\d ->
        case d of
          Left -> vertical (vec2 0 borderSize)
          Right -> vertical (vec2 (size - borderSize) borderSize)
          Top -> horizontal (vec2 borderSize 0)
          Bottom -> horizontal (vec2 borderSize (size - borderSize))
      )
    )
