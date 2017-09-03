module Components.Menu
    exposing
        ( MenuSection(..)
        , Menu
        , HomeItem(..)
        , MenuItem(..)
        , initial
        , update
        , Event(..)
        )

import Components.Keys as Keys exposing (Keys, codes)
import Time exposing (Time)


type Event
    = Noop
    | ToggleSound Bool
    | Start


type alias Menu =
    { time : Time
    , section : MenuSection
    }


type MenuSection
    = HomeSection HomeItem
    | MenuSection MenuItem
    | CreditsSection


type HomeItem
    = StartTheGame
    | GoToMenu


type MenuItem
    = SoundOnOff
    | GoToCredits


initial : Menu
initial =
    { time = 0
    , section = HomeSection StartTheGame
    }


goTo : MenuSection -> Menu -> Menu
goTo section menu =
    { menu | time = 0, section = section }


choose : MenuSection -> Menu -> Menu
choose section menu =
    { menu | section = section }


tick : Time -> Menu -> Menu
tick elapsed menu =
    { menu | time = menu.time + elapsed }


update : Time -> Bool -> Keys -> Menu -> ( Menu, Event )
update elapsed sound keys menu =
    case menu.section of
        HomeSection StartTheGame ->
            if Keys.pressed codes.enter keys then
                ( menu, Start )
            else if Keys.pressed codes.down keys then
                ( choose (HomeSection GoToMenu) menu, Noop )
            else
                ( tick elapsed menu, Noop )

        HomeSection GoToMenu ->
            if Keys.pressed codes.enter keys then
                ( goTo (MenuSection SoundOnOff) menu, Noop )
            else if Keys.pressed codes.up keys then
                ( choose (HomeSection StartTheGame) menu, Noop )
            else
                ( tick elapsed menu, Noop )

        MenuSection SoundOnOff ->
            if Keys.pressed codes.enter keys || Keys.pressed codes.left keys || Keys.pressed codes.right keys then
                ( menu, ToggleSound (not sound) )
            else if Keys.pressed codes.down keys then
                ( choose (MenuSection GoToCredits) menu, Noop )
            else if Keys.pressed codes.escape keys then
                ( goTo (HomeSection StartTheGame) menu, Noop )
            else
                ( tick elapsed menu, Noop )

        MenuSection GoToCredits ->
            if Keys.pressed codes.enter keys then
                ( goTo CreditsSection menu, Noop )
            else if Keys.pressed codes.up keys then
                ( choose (MenuSection SoundOnOff) menu, Noop )
            else if Keys.pressed codes.escape keys then
                ( goTo (HomeSection StartTheGame) menu, Noop )
            else
                ( tick elapsed menu, Noop )

        CreditsSection ->
            if Keys.pressed codes.escape keys then
                ( goTo (MenuSection SoundOnOff) menu, Noop )
            else
                ( tick elapsed menu, Noop )
