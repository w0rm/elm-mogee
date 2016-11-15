module View.Object exposing (render)

import Model.Object as Object exposing (Object, Category(..))
import WebGL as GL
import View.Common exposing (rectangle)
import View.Mogee as Mogee
import View.Wall as Wall


render : GL.Texture -> Object -> List GL.Renderable -> List GL.Renderable
render texture ({ position, category, velocity, size } as object) =
    case category of
        WallCategory ->
            (::) (Wall.render texture size ( Tuple.first position, Tuple.second position, 3 ))

        MogeeCategory mogee ->
            (::)
                (Mogee.render texture
                    position
                    mogee
                    (if Tuple.first velocity < 0 then
                        -1
                     else
                        1
                    )
                )

        ScreenCategory screen ->
            let
                monster =
                    Object.invertScreen object
            in
                (::) (rectangle size ( Tuple.first position, Tuple.second position, 5 ) ( 25, 30, 28 ))
                    >> (::) (rectangle monster.size ( Tuple.first monster.position, Tuple.second monster.position, 2 ) ( 22, 17, 22 ))
