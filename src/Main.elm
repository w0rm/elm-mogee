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
  WebGL.loadTexture "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAMAAACdt4HsAAAAFVBMVEXOzQH///9CTT+AjmkAAAAaHh3///+/VOlgAAAAB3RSTlP///////8AGksDRgAAAdpJREFUeNrtlttuwzAMQ5W45f9/8kSpHqEqaY0U6F6m2KYu1nGcDdgMRsOB4EQ2GhAB5dxwKFuax0jq7cZRJRWpT7IhB2EfAWCg6Rssy0bDNGZi5FwTtesNfEr13aWQFLNatRCO1qkrgLMD1oWfoAC4HAur2Tk39zfYx+42xuAMV8JBZSR3D5l9xvC+35m8eyJcd3bkHpanxZbhysm2u6/GbBiLI2rZiehBvAaCHv2DMgLG3bA9akhxKjXiAVCAgTiER4KRW3SMKJtnuMlzjyrVK9MbHAwGA8Vsoxhk5Tdl1RrATlFG6wBtFoDaNlvaEsD6ZiUXAFzaZkYnAOuAdFMFLWsH6OoRT1e54hUArQJaX3deAURoQOh+AqjWOkoBNNUF6FesPgqqX+FwD8LtNIVPACivegVo46NUAZAeEKAjBTv9DVdCgRK67SkANVEAvW7ohAirtUwHNFsGkPCuzV4BmvW7Ui8BZB8DcAkgAq4B9BkuArpt3wZssbwBbJ70yUWT4ZweU7MWEgkBoCeb6uEq6DkG5AmzK2wVoD55GS4Btt9DOWIWgup0Ugrgyo/wjwG6gf7SgEtqtfdciyY6+eh/U45FAJ/5Nsq4rgFkAOQuX+F1+R/wD/gS4Af9UkQ6nBJ6wwAAAABJRU5ErkJggg=="
  `Task.andThen`
  (Just >> Signal.send texture.address)
