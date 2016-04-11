module ObjectTests (all) where

import ElmTest exposing (..)
import Object exposing (collide, wall)


all : Test
all =
  suite "Object"
    [ test "collides with right side"
        ( assertEqual
            ( collide
                (wall (10, 10) (0, 0))
                (wall (10, 10) (2, 0))
            )
            True
        )
    , test "collides with bottom side"
        ( assertEqual
            ( collide
                (wall (10, 10) (0, 0))
                (wall (10, 10) (0, 2))
            )
            True
        )
    , test "doesn't collide"
        ( assertEqual
            ( collide
                (wall (2, 2) (0, 0))
                (wall (7, 10) (5, 5))
            )
            False
        )
    ]
