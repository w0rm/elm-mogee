module Screen (Screen, screen, walls) where

import Direction exposing (Direction(..))
import Object exposing (Object)


size : Float
size = 64


borderSize : Float
borderSize = 2


type alias Screen =
  { offset : (Float, Float) -- screen offset
  , direction : Direction -- direction to
  }


screen : (Float, Float) -> Direction -> Screen
screen = Screen


walls : Direction -> Screen -> List Object
walls from {offset, direction} =
  let
    (dx, dy) = offset
    corner (x, y) = Object.wall (borderSize, borderSize) (x + dx, y + dx)
    horizontal (x, y) = Object.wall (size - 2 * borderSize, borderSize) (x + dx, y + dx)
    vertical (x, y) = Object.wall (borderSize, size - 2 * borderSize) (x + dx, y + dx)
  in
    List.map
      corner
      [ (0, 0)
      , (size - borderSize, 0)
      , (size - borderSize, size - borderSize)
      , (0, size - borderSize)
      ]
    ++
    ( List.filter (\d -> d /= from && d /= direction) [Left, Top, Right, Bottom] |>
      List.map (\d ->
        case d of
          Left -> vertical (0, borderSize)
          Right -> vertical (size - borderSize, borderSize)
          Top -> horizontal (borderSize, 0)
          Bottom -> horizontal (borderSize, size - borderSize)
      )
    )
