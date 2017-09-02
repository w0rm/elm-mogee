module Components.Menu
    exposing
        ( Menu(..)
        , HomeItem(..)
        , MenuItem(..)
        , initial
        , update
        , Event(..)
        )

import Components.Keys as Keys exposing (Keys, codes)


type Event
    = Noop
    | ToggleSound Bool
    | Start


type Menu
    = Home HomeItem
    | Menu MenuItem
    | Credits


type HomeItem
    = StartTheGame
    | GoToMenu


type MenuItem
    = SoundOnOff
    | GoToCredits


initial : Menu
initial =
    Home StartTheGame


update : Bool -> Keys -> Menu -> ( Menu, Event )
update sound keys menu =
    case menu of
        Home StartTheGame ->
            if Keys.pressed codes.enter keys then
                ( menu, Start )
            else if Keys.pressed codes.down keys then
                ( Home GoToMenu, Noop )
            else
                ( menu, Noop )

        Home GoToMenu ->
            if Keys.pressed codes.enter keys then
                ( Menu SoundOnOff, Noop )
            else if Keys.pressed codes.up keys then
                ( Home StartTheGame, Noop )
            else
                ( menu, Noop )

        Menu SoundOnOff ->
            if Keys.pressed codes.enter keys || Keys.pressed codes.left keys || Keys.pressed codes.right keys then
                ( menu, ToggleSound (not sound) )
            else if Keys.pressed codes.down keys then
                ( Menu GoToCredits, Noop )
            else if Keys.pressed codes.escape keys then
                ( Home StartTheGame, Noop )
            else
                ( menu, Noop )

        Menu GoToCredits ->
            if Keys.pressed codes.enter keys then
                ( Credits, Noop )
            else if Keys.pressed codes.up keys then
                ( Menu SoundOnOff, Noop )
            else if Keys.pressed codes.escape keys then
                ( Home StartTheGame, Noop )
            else
                ( menu, Noop )

        Credits ->
            if Keys.pressed codes.escape keys then
                ( Menu SoundOnOff, Noop )
            else
                ( menu, Noop )
