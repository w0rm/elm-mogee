module View.Object exposing (render)

import Model.Object as Object exposing (Object, Category(..))
import View.Common exposing (rectangle)
import View.Mogee as Mogee
import View.Wall as Wall
import View.Screen as Screen
import View.Color as Color
import WebGL exposing (Texture, Entity)


render : Texture -> Object -> List Entity -> List Entity
render texture ({ position, category, velocity, size } as object) =
    case category of
        WallCategory ->
            (::) (Wall.render texture position size)

        MogeeCategory mogee ->
            (::) (Mogee.render texture position velocity mogee)

        ScreenCategory screen ->
            let
                monster =
                    Object.invertScreen object
            in
                (::) (rectangle size ( Tuple.first position, Tuple.second position, 5 ) Color.darkGreen)
                    >> (::) (rectangle monster.size ( Tuple.first monster.position, Tuple.second monster.position, 2 ) Color.darkBlue)
                    >> Screen.render texture monster.position size screen
