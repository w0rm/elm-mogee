import Graphics.Element exposing (Element)
import Keyboard
import Model
import Task exposing (Task)
import Time exposing (Time)
import View
import WebGL
import Window


main : Signal Element
main =
  Signal.map3
    View.view
    texture.signal
    size
    (Signal.foldp Model.update Model.model input)


input : Signal (Time, Model.Keys)
input =
  let
    delta = Time.fps 30
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)


size : Signal Int
size =
  Signal.map (\(x, y) -> min x y // 64 * 64) Window.dimensions


texture : Signal.Mailbox (Maybe WebGL.Texture)
texture =
  Signal.mailbox Nothing


port textureFetcher : Task WebGL.Error ()
port textureFetcher =
  WebGL.loadTexture "/texture.png"
  `Task.andThen`
  (Just >> Signal.send texture.address)
