module View.Color exposing (white, darkGreen, darkBlue, yellow, gray)

import Math.Vector3 as Vec3 exposing (Vec3)


toColor : Int -> Int -> Int -> Vec3
toColor r g b =
    Vec3.fromTuple ( toFloat r / 255, toFloat g / 255, toFloat b / 255 )


white : Vec3
white =
    toColor 255 255 255


darkGreen : Vec3
darkGreen =
    toColor 25 30 28


darkBlue : Vec3
darkBlue =
    toColor 22 17 22


yellow : Vec3
yellow =
    toColor 255 255 0


gray : Vec3
gray =
    toColor 100 100 100
