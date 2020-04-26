module Slides.Engine exposing (Element(..), Engine, Event(..), initial, update)

import Animation exposing (Animation)
import Components.Keys as Keys exposing (Keys, codes)
import Dict exposing (Dict)
import View.Font as Font exposing (Text)
import View.Sprite as Sprite exposing (Sprite)


type Event
    = AddText ElementId String { x : Float, y : Float }
    | AddSprite ElementId String { x : Float, y : Float }
    | Move ElementId Transition
    | Del ElementId
    | KeyPress


type ElementEvent
    = MoveElement Transition
    | DelElement


type alias ElementId =
    String


type Element
    = SpriteElement Sprite
    | TextElement Text


type alias ElementData =
    { element : Element
    , position :
        { x : Float
        , y : Float
        }
    , events : List ElementEvent
    }


type alias Transition =
    { x : Float
    , y : Float
    , duration : Float
    }


type alias Engine =
    { time : Float
    , elements : Dict ElementId ElementData
    , animations :
        Dict ElementId
            { x : Animation
            , y : Animation
            }
    , nextEvents : List Event
    , prevEvents : List Event
    }


initial : List Event -> Engine
initial events =
    { time = 0
    , elements = Dict.empty
    , animations = Dict.empty
    , nextEvents = events
    , prevEvents = []
    }


elementData : Element -> { x : Float, y : Float } -> ElementData
elementData element position =
    { element = element
    , position = position
    , events = []
    }


animate : Float -> Engine -> Engine
animate elapsed engine =
    Dict.foldl
        animateElement
        { engine | time = engine.time + elapsed }
        engine.elements


animateElement : ElementId -> ElementData -> Engine -> Engine
animateElement elementId data engine =
    case Dict.get elementId engine.animations of
        Just { x, y } ->
            if Animation.isDone engine.time x && Animation.isDone engine.time y then
                -- remove completed animation
                { engine | animations = Dict.remove elementId engine.animations }

            else
                -- running animation, do nothing
                engine

        Nothing ->
            -- schedule a new event
            scheduleElementEvent elementId data engine


scheduleElementEvent : ElementId -> ElementData -> Engine -> Engine
scheduleElementEvent elementId data engine =
    case data.events of
        (MoveElement { x, y, duration }) :: events ->
            { engine
                | animations =
                    Dict.insert elementId
                        { x =
                            Animation.animation engine.time
                                |> Animation.from data.position.x
                                |> Animation.to x
                                |> Animation.duration duration
                        , y =
                            Animation.animation engine.time
                                |> Animation.from data.position.y
                                |> Animation.to y
                                |> Animation.duration duration
                        }
                        engine.animations
                , elements =
                    Dict.insert elementId
                        { data | events = events, position = { x = x, y = y } }
                        engine.elements
            }

        DelElement :: _ ->
            { engine | elements = Dict.remove elementId engine.elements }

        [] ->
            engine


update : Float -> Keys -> Engine -> Engine
update elapsed keys engine =
    updateNext keys (animate elapsed engine)


noAnimations : Engine -> Bool
noAnimations =
    .animations >> Dict.isEmpty


updateNext : Keys -> Engine -> Engine
updateNext keys engine =
    case engine.nextEvents of
        ((AddSprite elementId element position) as event) :: events ->
            { engine
                | nextEvents = events
                , prevEvents = event :: engine.prevEvents
                , elements = Dict.insert elementId (elementData (SpriteElement (Sprite.sprite element)) position) engine.elements
            }

        ((AddText elementId element position) as event) :: events ->
            { engine
                | nextEvents = events
                , prevEvents = event :: engine.prevEvents
                , elements = Dict.insert elementId (elementData (TextElement (Font.text element)) position) engine.elements
            }

        ((Move elementId transition) as event) :: events ->
            { engine
                | nextEvents = events
                , prevEvents = event :: engine.prevEvents
                , elements = addElementEvent elementId (MoveElement transition) engine.elements
            }

        ((Del elementId) as event) :: events ->
            { engine
                | nextEvents = events
                , prevEvents = event :: engine.prevEvents
                , elements = addElementEvent elementId DelElement engine.elements
            }

        (KeyPress as event) :: events ->
            if Keys.pressed codes.right keys then
                if noAnimations engine then
                    { engine
                        | nextEvents = events
                        , prevEvents = event :: engine.prevEvents
                    }

                else
                    runEvents (List.reverse engine.prevEvents)
                        { engine
                            | elements = Dict.empty
                            , animations = Dict.empty
                        }

            else if Keys.pressed codes.left keys then
                updatePrev engine

            else
                engine

        [] ->
            if Keys.pressed codes.left keys then
                updatePrev engine

            else
                engine


updatePrev : Engine -> Engine
updatePrev engine =
    case engine.prevEvents of
        KeyPress :: events ->
            runEvents (List.reverse events)
                { engine
                    | elements = Dict.empty
                    , animations = Dict.empty
                    , prevEvents = events
                    , nextEvents = KeyPress :: engine.nextEvents
                }

        event :: events ->
            updatePrev
                { engine
                    | prevEvents = events
                    , nextEvents = event :: engine.nextEvents
                }

        [] ->
            { engine
                | elements = Dict.empty
                , animations = Dict.empty
            }


runEvents : List Event -> Engine -> Engine
runEvents events engine =
    case events of
        (AddSprite elementId element position) :: restEvents ->
            runEvents restEvents { engine | elements = Dict.insert elementId (elementData (SpriteElement (Sprite.sprite element)) position) engine.elements }

        (AddText elementId element position) :: restEvents ->
            runEvents restEvents { engine | elements = Dict.insert elementId (elementData (TextElement (Font.text element)) position) engine.elements }

        (Move elementId { x, y }) :: restEvents ->
            runEvents restEvents
                { engine
                    | elements =
                        Dict.update
                            elementId
                            (\maybeData ->
                                case maybeData of
                                    Just data ->
                                        let
                                            position =
                                                { x = x, y = y }
                                        in
                                        Just { data | position = position }

                                    Nothing ->
                                        Nothing
                            )
                            engine.elements
                }

        (Del elementId) :: restEvents ->
            runEvents restEvents { engine | elements = Dict.remove elementId engine.elements }

        KeyPress :: restEvents ->
            runEvents restEvents engine

        [] ->
            engine


{-| adds an event to an element, if the element exists in a dict
-}
addElementEvent : ElementId -> ElementEvent -> Dict ElementId ElementData -> Dict ElementId ElementData
addElementEvent elementId elementEvent =
    Dict.update elementId
        (\val ->
            case val of
                Just data ->
                    Just { data | events = data.events ++ [ elementEvent ] }

                Nothing ->
                    Nothing
        )
