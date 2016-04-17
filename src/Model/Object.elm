module Model.Object (Object, Category(..), update, wall, mogee, isMogee, collide, offset) where

import Model.Mogee as Mogee exposing (Mogee)
import Time exposing (Time)


type Category
  = WallCategory
  | MogeeCategory Mogee


type alias Keys =
  { x : Int
  , y : Int
  }


type alias Object =
  { category : Category
  , velocity : (Float, Float)
  , size : (Float, Float) -- dimensions
  , position : (Float, Float) -- the top left corner
  }


offset : (Float, Float) -> Object -> Object
offset (dx, dy) object =
  { object
  | position = (fst object.position - dx, snd object.position - dy)
  }


gravity : Float
gravity = 0.0001


friction : Float
friction = 0.001


jumpVelocity : Float
jumpVelocity = 0.06


walkVelocity : Float
walkVelocity = 0.03


mogee : (Float, Float) -> Object
mogee =
  Object (MogeeCategory Mogee.mogee) (0, 0) Mogee.size


wall : (Float, Float) -> (Float, Float) -> Object
wall =
  Object WallCategory (0, 0)


isMogee : Object -> Bool
isMogee obj =
  case obj.category of
    MogeeCategory _ -> True
    _ -> False


isWall : Object -> Bool
isWall obj =
  case obj.category of
    WallCategory -> True
    _ -> False


moveY : Time -> Float -> List Object -> Object -> Object
moveY dt dy objects object =
  let
    (vx, vy) = object.velocity
    (x, y) = object.position
    newVelocity = vy + gravity * dt
    deltaY = dt * (vy + newVelocity) * 0.5
    newObject =
      { object
      | position = (x, y + deltaY)
      , velocity = (vx, newVelocity)
      }
    collisions = List.filter (collide newObject) objects
  in
    case List.head collisions of
      Nothing ->
        newObject
      Just {position, size} ->
        if deltaY < 0 then
          {- Hit the top wall -}
          { object
          | velocity = (vx, -vy)
          , position = (x, snd position + snd size)
          }
        else
          {- Hit the bottom wall -}
          { object
          | velocity = (vx, if dy == 1 then -jumpVelocity else 0)
          , position = (x, snd position - snd object.size)
          }


moveX : Time -> Float -> List Object -> Object -> Object
moveX dt dx objects object =
  let
    (vx, vy) = object.velocity
    (x, y) = object.position
    newVelocity =
      if dx == 0 then
        if vx /= 0 then
          let
            new = (abs vx - friction * dt)
          in
            if new > 0 then
              (vx / abs vx) * new
            else
              0
        else
          0
      else
        dx * walkVelocity
    deltaX = dt * (vx + newVelocity) * 0.5
    newObject =
      { object
      | position = (x + deltaX, y)
      , velocity = (newVelocity, vy)
      }
    collisions = List.filter (collide newObject) objects
  in
    case List.head collisions of
      Nothing ->
        newObject
      Just {position, size} ->
        if deltaX < 0 then
          {- Hit the left wall -}
          { object
          | position = (fst position + fst size, y)
          , velocity = (0, vy)
          }
        else
          {- Hit the right wall -}
          { object
          | position = (fst position - fst object.size, y)
          , velocity = (0, vy)
          }


update : (Time, Keys) -> List Object -> Object -> Object
update (dt, {x, y}) objects object =
  case object.category of
    WallCategory ->
      object
    MogeeCategory mogee ->
      let
        rest = List.filter ((/=) object) objects
      in
        { object
        | category = MogeeCategory (Mogee.update dt object.velocity mogee)
        }
          |> moveY dt (toFloat y) rest
          |> moveX dt (toFloat x) rest


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
