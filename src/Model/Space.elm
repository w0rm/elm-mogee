module Model.Space (Space, space) where

import Model.Direction exposing (Direction(..))


type alias Space =
  { direction : Direction
  , active : Bool
  }


space : Direction -> Bool -> Space
space dir act =
  { direction = dir
  , active = act
  }
