module Actions exposing
  ( Action(..)
  )


import Window exposing (Size)
import Time exposing (Time)
import Model.Keys exposing (Keys)
import WebGL exposing (Error, Texture)


type Action
  = Resize Size
  | Animate Time
  | KeyChange (Keys -> Keys)
  | TextureError Error
  | TextureLoaded Texture
