module Components.Menu exposing
    ( Event(..)
    , HomeItem(..)
    , Menu
    , MenuItem(..)
    , MenuSection(..)
    , PauseItem(..)
    , paused
    , start
    , update
    )

import Components.Keys as Keys exposing (Keys, codes)


type Event
    = Noop
    | ToggleSound Bool
    | Start
    | Action
    | Resume
    | End


type alias Menu =
    { time : Float
    , section : MenuSection
    }


type MenuSection
    = HomeSection HomeItem
    | MenuSection MenuItem
    | PauseSection PauseItem
    | CreditsSection
    | SlidesSection


type HomeItem
    = StartTheGame
    | GoToMenu


type MenuItem
    = SoundOnOff
    | GoToCredits
    | GoToSlides


type PauseItem
    = EndGame
    | ResumeGame


start : Menu
start =
    { time = 0
    , section = HomeSection StartTheGame
    }


paused : Menu
paused =
    { time = 0
    , section = PauseSection ResumeGame
    }


goTo : MenuSection -> Menu -> Menu
goTo section menu =
    { menu | time = 0, section = section }


choose : MenuSection -> Menu -> Menu
choose section menu =
    { menu | section = section }


tick : Float -> Menu -> Menu
tick elapsed menu =
    { menu | time = menu.time + elapsed }


update : Float -> Bool -> Keys -> Menu -> ( Menu, Event )
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
                ( goTo (MenuSection SoundOnOff) menu, Action )

            else if Keys.pressed codes.up keys then
                ( choose (HomeSection StartTheGame) menu, Noop )

            else
                ( tick elapsed menu, Noop )

        MenuSection SoundOnOff ->
            if Keys.pressed codes.enter keys || Keys.pressed codes.left keys || Keys.pressed codes.right keys then
                ( menu, ToggleSound (not sound) )

            else if Keys.pressed codes.down keys then
                ( choose (MenuSection GoToCredits) menu, Noop )

            else if Keys.pressed codes.escape keys || Keys.pressed codes.q keys then
                ( goTo (HomeSection StartTheGame) menu, Action )

            else
                ( tick elapsed menu, Noop )

        MenuSection GoToCredits ->
            if Keys.pressed codes.enter keys then
                ( goTo CreditsSection menu, Action )

            else if Keys.pressed codes.up keys then
                ( choose (MenuSection SoundOnOff) menu, Noop )

            else if Keys.pressed codes.down keys then
                ( choose (MenuSection GoToSlides) menu, Noop )

            else if Keys.pressed codes.escape keys || Keys.pressed codes.q keys then
                ( goTo (HomeSection StartTheGame) menu, Action )

            else
                ( tick elapsed menu, Noop )

        MenuSection GoToSlides ->
            if Keys.pressed codes.enter keys then
                ( goTo SlidesSection menu, Action )

            else if Keys.pressed codes.up keys then
                ( choose (MenuSection GoToCredits) menu, Noop )

            else if Keys.pressed codes.escape keys || Keys.pressed codes.q keys then
                ( goTo (HomeSection StartTheGame) menu, Action )

            else
                ( tick elapsed menu, Noop )

        CreditsSection ->
            if Keys.pressed codes.escape keys || Keys.pressed codes.q keys then
                ( goTo (MenuSection GoToCredits) menu, Action )

            else
                ( tick elapsed menu, Noop )

        SlidesSection ->
            if Keys.pressed codes.escape keys || Keys.pressed codes.q keys then
                ( goTo (MenuSection GoToSlides) menu, Action )

            else
                ( tick elapsed menu, Noop )

        PauseSection ResumeGame ->
            if Keys.pressed codes.enter keys then
                ( menu, Resume )

            else if Keys.pressed codes.down keys then
                ( choose (PauseSection EndGame) menu, Noop )

            else if Keys.pressed codes.escape keys || Keys.pressed codes.q keys then
                ( menu, Resume )

            else
                ( tick elapsed menu, Noop )

        PauseSection EndGame ->
            if Keys.pressed codes.enter keys then
                ( menu, End )

            else if Keys.pressed codes.up keys then
                ( choose (PauseSection ResumeGame) menu, Noop )

            else if Keys.pressed codes.escape keys || Keys.pressed codes.q keys then
                ( menu, Resume )

            else
                ( tick elapsed menu, Noop )
