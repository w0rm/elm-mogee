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


input : Signal (Time, Model.Keys, Bool)
input =
  Signal.sampleOn fps60 (Signal.map3 (,,) fps60 Keyboard.arrows Keyboard.enter)


size : Signal Int
size =
  Signal.map (\(x, y) -> min x y // 64 * 64) Window.dimensions


texture : Signal.Mailbox (Maybe WebGL.Texture)
texture =
  Signal.mailbox Nothing


port textureFetcher : Task WebGL.Error ()
port textureFetcher =
  WebGL.loadTexture "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAMAAACdt4HsAAAAHlBMVEXOzQH///9CTT+AjmkAAAAaHh3OzwDPzwDOzgAAAADlbZFbAAAACnRSTlP///////////8AsswszwAAAcNJREFUeNrtkuGSgyAMhINU233/Fz426O3EaMvYm+ufboElhHyI1mAUDgwnVihgWRZw8YlwaKXrgQcWB9xubNG6o/vOCnq73+94C4AFlN7BsBUKmzzB1vuYqVxPYF37ryCHLMhi1tzYUqWuAPYMGDe+ggDgcGjsa6U2759gqlNTrZXdpzI2OiNNJ7etzhjO08zFuS34dG4p9D1Mb+IWDpWdZXMbjasuJv0MNhK8Bv4YcLrXV1p1GHfDJs+hW6PSPa4ADajwQ3gkGDV5RfW0oYKb2tqapTOzziobg8pAcV3TBin8U0aVAHaKMioDtFkAetpsXUMAy5u1OADgkDYzOgFYBvSp3A1hzABd3eNtqrUwCwAqAlJdnjwDiJCA0P0EUC5VhAQo5QXIV4xzBFS+wuEe+DTTFO4A2AOoCNDGNRUBkB8QoCMFO/2Ha0GBFnTbUwDiQgDkvCETYqgbZ2WANA4g4VWZPQMk5bvSLwGktwG4BBAB1wB6DRcBWeW/AcWHF4BSgNY5qDPcein0NefmCwJAv14UD1ci/DKAJ4lQXKMA1WnWwyFA+T2UzXsgKM9JtwC48gk/DSgFkuGKvoAv4Av4e8APsI5nfrKJSK0AAAAASUVORK5CYII="
  `Task.andThen`
  (Just >> Signal.send texture.address)
