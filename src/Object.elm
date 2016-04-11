module Object (Object, Category(..), update, wall, mogee, collide) where

import Mogee exposing (Mogee)
import Time exposing (Time)


type Category
  = WallCategory
  | MogeeCategory Mogee


type alias Keys =
  { x : Int
  , y : Int
  }


type Physics
  = Static
  | Gravity Float


type alias Object =
  { category : Category
  , physics : Physics
  , size : (Float, Float) -- dimensions
  , position : (Float, Float) -- the top left corner
  }


gravity : Float
gravity = 0.0001


jumpVelocity : Float
jumpVelocity = -0.05


walkVelocity : Float
walkVelocity = 0.03


mogee : (Float, Float) -> Object
mogee =
  Object (MogeeCategory Mogee.mogee) (Gravity 0) (7, 10)


wall : (Float, Float) -> (Float, Float) -> Object
wall =
  Object WallCategory Static


moveY : Time -> Float -> Float -> List Object -> Object -> Object
moveY dt dy velocity objects object =
  let
    newVelocity = velocity + gravity * dt
    deltaY = dt * (velocity + newVelocity) * 0.5
    newObject =
      { object
      | position = (fst object.position, snd object.position + deltaY)
      , physics = Gravity newVelocity
      }
    collisions = List.filter (collide newObject) objects
    x = fst object.position
  in
    case List.head collisions of
      Nothing -> newObject
      Just {position, size} ->
        if deltaY < 0 then
          {- Jumping up -}
          { object
          | physics = Gravity -velocity
          , position = (x, snd position + snd size)
          }
        else
          {- Falling down -}
          { object
          | physics = if dy == 1 then Gravity jumpVelocity else Gravity 0
          , position = (x, snd position - snd object.size)
          }


moveX : Time -> Float -> List Object -> Object -> Object
moveX dt dx objects object =
  let
    deltaX = dt * dx * walkVelocity
    newObject =
      { object
      | position = (fst object.position + deltaX, snd object.position)
      }
    collisions = List.filter (collide newObject) objects
    y = snd object.position
  in
    case List.head collisions of
      Nothing -> newObject
      Just {position, size} ->
        if deltaX < 0 then
          {- Hit the left wall -}
          { object
          | position = (fst position + fst size, y)
          }
        else
          {- Hit the right wall -}
          { object
          | position = (fst position - fst object.size, y)
          }


update : (Time, Keys) -> List Object -> Object -> Object
update (dt, keys) objects object =
  case object.physics of
    Static ->
      object
    Gravity velocity ->
      let
        restObjects = List.filter ((/=) object) objects
      in
        object
        |> moveY dt (toFloat keys.y) velocity restObjects
        |> moveX dt (toFloat keys.x) restObjects


collide : Object -> Object -> Bool
collide o1 o2 =
  let
    (x1, y1) = o1.position
    (w1, h1) = o1.size
    (x2, y2) = o2.position
    (w2, h2) = o2.size
  in
    x1 < x2 + w2 && x1 + w1 > x2 &&
    y1 < y2 + h2 && y1 + h1 > y2
