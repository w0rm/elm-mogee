module View (view) where

import WebGL as GL
import Graphics.Element exposing (Element)
import Model exposing (Model)
import View.Common as Common
import View.Object as Object
import Model.Object exposing (invertSpace, isSpace)
import View.Lives as Lives


view : Maybe GL.Texture -> Int -> Model -> Element
view maybeTexture size model =
  GL.webglWithConfig
    [ GL.Enable GL.Blend
    , GL.BlendFunc (GL.One, GL.OneMinusSrcAlpha)
    ]
    (size, size)
    ( case maybeTexture of
        Nothing ->
          []
        Just texture ->
          render texture model
          |> List.sortBy fst
          |> List.map snd
    )


toMinimap : (Float, Float) -> (Float, Float)
toMinimap (x, y) =
  ( floor (x / 64) |> toFloat
  , floor (y / 64) |> toFloat
  )


render : GL.Texture -> Model -> List (Int, GL.Renderable)
render texture model =
  let
    (x, y) = Model.mogee model |> .position

    offset = (x - 32 + 4, y - 32 + 5)

    (cx, cy) = toMinimap (x, y)

    allScr = model.objects
      |> List.filter isSpace
      |> List.map (.position >> toMinimap)

    maxX = List.maximum (List.map fst allScr) |> Maybe.withDefault 0
    minY = List.minimum (List.map snd allScr) |> Maybe.withDefault 0

    dot (x1, y1) =
      Common.rectangle
        (1, 1)
        (63 - maxX - 1 + x1, y1 - minY + 1)
        ( if x1 == cx && y1 == cy then
            (255, 255, 0)
          else
            (100, 100, 100)
        )
      |> (,) 0

    bg = (6, Common.rectangle (64, 64) (0, 0) (22, 17, 22))

    monster {position, size} =
      (2, Common.rectangle size (fst position - fst offset, snd position - snd offset) (22, 17, 22))

    offsetObject ({position} as object) =
      { object
      | position = (fst position - fst offset, snd position - snd offset)
      }

  in
    if model.state == Model.Stopped then
      (if model.score > 0 then (Lives.renderScore texture (32, 1) model.score) else []) ++
      [ Lives.renderTitle texture (3, 14)
      , Lives.renderPlay texture (5, 44)
      , bg
      ]
    else
      (if model.state == Model.Paused then [Lives.renderPlay texture (5, 44)] else []) ++
      Lives.render texture (1, 1) model.lives ++
      Lives.renderScore texture (32, 1) (model.currentScore + model.score) ++
      List.map dot allScr ++
      List.foldl (Object.render texture) [bg] (List.map offsetObject model.objects)
