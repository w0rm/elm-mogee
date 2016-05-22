module View.Object exposing (render)

import Model.Object as Object exposing (Object, Category(..))
import WebGL as GL
import View.Common exposing (rectangle)
import View.Mogee as Mogee
import View.Wall as Wall


render : GL.Texture -> Object -> List (Int, GL.Renderable) -> List (Int, GL.Renderable)
render texture ({position, category, velocity, size} as object) =
  case category of
    WallCategory ->
      (::) (3, Wall.render texture size position)
    MogeeCategory mogee ->
      (::) (Mogee.render texture position mogee (if fst velocity < 0 then -1 else 1))
    ScreenCategory screen ->
      let
        monster = Object.invertScreen object
      in
        (::) (5, rectangle size position (25, 30, 28)) >>
        (::) (2, rectangle monster.size monster.position (22, 17, 22))
