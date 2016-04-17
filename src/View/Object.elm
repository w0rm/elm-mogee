module View.Object (render) where

import Model.Object exposing (Object, Category(..))
import WebGL as GL
import View.Common exposing (rectangle)
import View.Mogee as Mogee
import View.Wall as Wall


render : GL.Texture -> (Float, Float) -> Object -> List (Int, GL.Renderable) -> List (Int, GL.Renderable)
render texture offset {position, category, velocity, size} =
  let
    pos = (fst position - fst offset, snd position - snd offset)
  in
    case category of
      WallCategory ->
        (::) (3, Wall.render texture size pos)
      MogeeCategory mogee ->
        (::) (Mogee.render texture pos mogee (if fst velocity < 0 then -1 else 1))
      SpaceCategory space ->
        (::) (5, rectangle size pos (25, 30, 28))
