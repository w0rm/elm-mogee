module Main exposing (..)

import WebGL
import Keyboard
import Model exposing (Model)
import Model.Keys as Keys
import Task exposing (Task)
import Actions exposing (Action(..))
import View
import Window
import AnimationFrame
import Html


subscriptions : Model -> Sub Action
subscriptions _ =
    [ AnimationFrame.diffs Animate
    , Keyboard.downs (Keys.keyChange True >> KeyChange)
    , Keyboard.ups (Keys.keyChange False >> KeyChange)
    , Window.resizes Resize
    ]
        |> Sub.batch


init : ( Model, Cmd Action )
init =
    ( Model.model
    , Cmd.batch
        [ WebGL.loadTexture "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAMAAACdt4HsAAAAFVBMVEXOzQH///9CTT+AjmkAAAAaHh3///+/VOlgAAAAB3RSTlP///////8AGksDRgAAAdpJREFUeNrtlttuwzAMQ5W45f9/8kSpHqEqaY0U6F6m2KYu1nGcDdgMRsOB4EQ2GhAB5dxwKFuax0jq7cZRJRWpT7IhB2EfAWCg6Rssy0bDNGZi5FwTtesNfEr13aWQFLNatRCO1qkrgLMD1oWfoAC4HAur2Tk39zfYx+42xuAMV8JBZSR3D5l9xvC+35m8eyJcd3bkHpanxZbhysm2u6/GbBiLI2rZiehBvAaCHv2DMgLG3bA9akhxKjXiAVCAgTiER4KRW3SMKJtnuMlzjyrVK9MbHAwGA8Vsoxhk5Tdl1RrATlFG6wBtFoDaNlvaEsD6ZiUXAFzaZkYnAOuAdFMFLWsH6OoRT1e54hUArQJaX3deAURoQOh+AqjWOkoBNNUF6FesPgqqX+FwD8LtNIVPACivegVo46NUAZAeEKAjBTv9DVdCgRK67SkANVEAvW7ohAirtUwHNFsGkPCuzV4BmvW7Ui8BZB8DcAkgAq4B9BkuArpt3wZssbwBbJ70yUWT4ZweU7MWEgkBoCeb6uEq6DkG5AmzK2wVoD55GS4Btt9DOWIWgup0Ugrgyo/wjwG6gf7SgEtqtfdciyY6+eh/U45FAJ/5Nsq4rgFkAOQuX+F1+R/wD/gS4Af9UkQ6nBJ6wwAAAABJRU5ErkJggg=="
            |> Task.attempt
                (\result ->
                    case result of
                        Err err ->
                            TextureError err

                        Ok val ->
                            TextureLoaded val
                )
        , Task.perform Resize Window.size
        ]
    )


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = View.view
        , subscriptions = subscriptions
        , update = Model.update
        }
