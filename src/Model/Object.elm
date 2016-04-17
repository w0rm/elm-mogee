module Model.Object (Object, Category(..), isDead, update, wall, mogee, space, isMogee, isSpace, collide, offset, invertSpace) where

import Model.Mogee as Mogee exposing (Mogee)
import Model.Space as Space exposing (Space)
import Time exposing (Time)
import Model.Direction exposing (Direction(..))


type Category
  = WallCategory
  | MogeeCategory Mogee
  | SpaceCategory Space


type alias Keys =
  { x : Int
  , y : Int
  }


type alias Object =
  { category : Category
  , number : Int -- screen number
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


spaceVelocity : Float
spaceVelocity = 0.01


space : Direction -> Int -> (Float, Float) -> Object
space direction number =
  Object
    (SpaceCategory (Space.space direction (number == 0)))
    number
    (spaceVelocity + 0.001 * number, spaceVelocity + 0.001 * number)
    (64, 64)


mogee : (Float, Float) -> Object
mogee =
  Object (MogeeCategory Mogee.mogee) 0 (0, 0) Mogee.size


wall : Int -> (Float, Float) -> (Float, Float) -> Object
wall number =
  Object WallCategory number (0, 0)


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


isSpace : Object -> Bool
isSpace obj =
  case obj.category of
    SpaceCategory _ -> True
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


invertSpace : Object -> Maybe Object
invertSpace ({size, position, category} as object) =
  case category of
    SpaceCategory {direction, active} ->
      let
        (x, y) = position
        (w, h) = size
      in
        if active then
          Just ( case direction of
            Left ->
              { object
              | size = (64 - w, h)
              , position = (x + w, y)
              }
            Right ->
              { object
              | size = (64 - w, h)
              , position = (x - (64 - w), y)
              }
            Top ->
              { object
              | size = (w, 64 - h)
              , position = (x, y + h)
              }
            Bottom ->
              { object
              | size = (w, 64 - h)
              , position = (x, y - (64 - h))
              }
          )
        else
          Nothing
    _ -> Nothing


shrink : Time -> Direction -> Object -> Object
shrink dt direction object =
  let
    (x, y) = object.position
    (w, h) = object.size
    newW = max 0 (w - dt * fst object.velocity)
    newH = max 0 (h - dt * snd object.velocity)
  in
    if w == 0 || h == 0 then
      object
    else
      case direction of
        Left ->
          { object
          | size = (newW, 64)
          }
        Right ->
          { object
          | size = (newW, 64)
          , position = (x - newW + w, y)
          }
        Top ->
          { object
          | size = (64, newH)
          }
        Bottom ->
          { object
          | size = (64, newH)
          , position = (x, y - newH + h)
          }


activate : Space -> List Object -> Object -> Object
activate space objects object =
  let
    isPrevious {category, size, number} =
      (fst size == 0 || snd size == 0) && number == object.number - 1
    newSpace =
      if List.any isPrevious objects then
        { space | active = True }
      else
        space
  in
    { object | category = SpaceCategory newSpace }


update : (Time, Keys) -> List Object -> Object -> List Object -> List Object
update (dt, {x, y}) objects object =
  case object.category of
    SpaceCategory space ->
      if space.active then
        shrink dt space.direction object |> (::)
      else
        activate space (List.filter isSpace objects) object |> (::)
    WallCategory ->
      if List.any (collide object) (List.filter isSpace objects) then
        object |> (::)
      else
        identity
    MogeeCategory mogee ->
      let
        rest = List.filter isWall objects
      in
        if List.any (collide object) (List.filter isSpace objects) then
          { object
          | category = MogeeCategory (Mogee.update dt object.velocity mogee)
          }
            |> moveY dt (toFloat y) rest
            |> moveX dt (toFloat x) rest
            |> (::)
        else
          { object
          | category = MogeeCategory (Mogee.die mogee)
          }
            |> (::)


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


expandSpaces : List Object -> List (Object, Space)
expandSpaces =
  List.filterMap
    (\o ->
      case o.category of
        SpaceCategory space -> Just (o, space)
        _ -> Nothing
    )


isDead : List Object -> Bool
isDead =
  List.any
    (\{category} ->
      case category of
        MogeeCategory {state} -> state == Mogee.Dead
        _ -> False
    )
