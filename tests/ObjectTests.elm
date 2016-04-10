module ObjectTests (all) where

import ElmTest exposing (..)
import Object
import Math.Vector2 exposing (Vec2, vec2)



all : Test
all =
  suite "Object"
    [ test "collides with right side"
        ( assertEqual
            ( Object.collide
                (Object.wall (vec2 10 10) (vec2 0 0))
                (Object.wall (vec2 10 10) (vec2 2 0))
            )
            True
        )
    , test "collides with bottom side"
        ( assertEqual
            ( Object.collide
                (Object.wall (vec2 10 10) (vec2 0 0))
                (Object.wall (vec2 10 10) (vec2 0 2))
            )
            True
        )
    , test "doesn't collide"
        ( assertEqual
            ( Object.collide
                (Object.wall (vec2 2 2) (vec2 0 0))
                (Object.wall (vec2 7 10) (vec2 5 5))
            )
            False
        )
    ]
