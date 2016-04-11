import Graphics.Element exposing (Element)
import Keyboard
import Model
import Task exposing (Task)
import Time exposing (Time)
import View
import WebGL
import Window
import Effects exposing (Effects)


animation : Signal.Mailbox (List Time)
animation =
  Signal.mailbox []


port runAnimation : Signal (Task.Task Effects.Never ())
port runAnimation =
  Signal.map (Effects.toTask animation.address << snd) time


time : Signal (Time, Effects Time)
time =
  Signal.foldp
    (\t2 (t1, effect) -> (min (t2 - t1) 25, effect))
    (0, Effects.tick identity)
    (Signal.filterMap List.head 0 animation.signal)


{- A better `Time.fps 60` -}
fps60 : Signal Time
fps60 =
  Signal.map fst time


main : Signal Element
main =
  Signal.map3
    View.view
    texture.signal
    size
    (Signal.foldp Model.update Model.model input)


input : Signal (Time, Model.Keys)
input =
  Signal.sampleOn fps60 (Signal.map2 (,) fps60 Keyboard.arrows)


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
