module Slides.Slides exposing (initial)

import Slides.Engine as Engine exposing (..)


titleSlide : List Event
titleSlide =
    [ AddText "title1" "Mogee or how" { x = 7, y = 13 }
    , AddText "title2" "we fit Elm" { x = 13, y = 24 }
    , AddText "title3" "in a 64×64 grid" { x = 4, y = 35 }
    , KeyPress
    , Move "title2" { x = -41, y = 24, duration = 1000 }
    , Del "title2"
    , AddText "title4" "we fit slides" { x = 64, y = 24 }
    , Move "title4" { x = 10, y = 24, duration = 1000 }
    , KeyPress
    , Del "title1"
    , Del "title3"
    , Del "title4"
    ]


carmackSlide : List Event
carmackSlide =
    [ AddSprite "carmackCarmack" "carmack" { x = 21, y = 10 }
    , AddText "carmack1" "John D. Carmack" { x = 3, y = 42 }
    , KeyPress
    , Del "carmackCarmack"
    , Del "carmack1"
    ]


{-| color twitter and soundcloud
-}
aboutSlide : List Event
aboutSlide =
    [ AddText "about1" "Andrey Kuzmin\n@unsoundscapes\n     SoundCloud" { x = 2, y = 23 }
    , AddSprite "aboutSoundcloud" "soundcloud" { x = 2, y = 47 }
    , KeyPress
    , Del "about1"
    , Del "aboutSoundcloud"
    ]


whyGamesInElmSlide : List Event
whyGamesInElmSlide =
    [ AddText "why1" "Why games" { x = 10, y = 17 }
    , AddText "why2" "in      ?" { x = 25, y = 32 }
    , AddSprite "whyElm" "elm" { x = 34, y = 30 }
    , KeyPress
    , Del "why1"
    , Del "why2"
    , Del "whyElm"
    ]


myGamesInElmSlide : List Event
myGamesInElmSlide =
    [ AddText "myGames1" "elm-flatris" { x = 2, y = 2 }
    , AddText "myGames2" "elm-street-\n404" { x = 2, y = 22 }
    , AddSprite "myGamesElmStreet404" "elm-street-404" { x = 40, y = 39 }
    , KeyPress
    , Del "myGames1"
    , Del "myGames2"
    , Del "myGamesElmStreet404"
    ]


challengesSlide : List Event
challengesSlide =
    [ AddText "challenges1" "Challenges:\n1. Game State\n2. Interactions\n3. Rendering" { x = 2, y = 2 }
    , KeyPress
    , Del "challenges1"
    ]


gameStateSlide : List Event
gameStateSlide =
    [ AddText "gameState1" "1. Game State" { x = 6, y = 21 }
    , KeyPress
    , Del "gameState1"
    ]


marioModelSlide : List Event
marioModelSlide =
    [ AddSprite "marioJump" "mario-jump" { x = 44, y = 38 }
    , Move "marioJump" { x = 44, y = 0, duration = 1000 }
    , Move "marioJump" { x = 44, y = 38, duration = 1000 }
    , KeyPress
    , Del "marioJump"
    , AddSprite "marioStay" "mario" { x = 45, y = 38 }
    , AddText "mario1" "{ dir: Direction,\n  vx: Float,\n  vy: Float,\n  x: Float,\n  y: Float }" { x = 2, y = 2 }
    , KeyPress
    , Del "marioStay"
    , Del "mario1"
    ]


elmStreet404ModelSlide : List Event
elmStreet404ModelSlide =
    [ AddSprite "elmStreet404" "elm-street-404" { x = 43, y = 3 }
    , AddText "elmStreet4041" "{ w: List\n     Warehouse,\n o: List Obstacle,\n h: List House }" { x = 2, y = 18 }
    , KeyPress
    , Del "elmStreet4041"
    , AddText "elmStreet4042" "type alias\nEntity =\n{ s: Size,\n  p: Position,\n  cat: Category }" { x = 2, y = 7 }
    , KeyPress
    , Del "elmStreet4042"
    , AddText "elmStreet4043" "type \nCategory \n= House Int\n | Warehouse Int\n | Obstacle Data" { x = 2, y = 7 }
    , KeyPress
    , Del "elmStreet4043"
    , Del "elmStreet404"
    ]


mogeeModelSlide : List Event
mogeeModelSlide =
    [ AddSprite "mogeeMogee" "mogee" { x = 45, y = 8 }
    , AddText "mogee1" "type \nCategory \n= Wall\n | Mogee Data1\n | Screen Data2" { x = 2, y = 7 }
    , KeyPress
    , Del "mogeeMogee"
    , Del "mogee1"
    ]


ecsSlide : List Event
ecsSlide =
    [ AddText "ecs1" "Entity-\nComponent-\nSystem" { x = 2, y = 7 }
    , KeyPress
    , Del "ecs1"
    , AddText "ecs2" "type alias\nComponents =\n{ id: Int,\n  a: Dict Int A,\n  b: Dict Int B }" { x = 2, y = 2 }
    , KeyPress
    , Del "ecs2"
    , AddSprite "escMogee" "mogee" { x = 48, y = 46 }
    , AddText "ecs3" "Components:\n Mogee, Screen,\n Wall, Velocity,\n Transform" { x = 2, y = 2 }
    , KeyPress
    , Del "ecs3"
    , Del "escMogee"
    ]


interactionsSlide : List Event
interactionsSlide =
    [ AddText "interactions1" "2. Interactions" { x = 5, y = 21 }
    , KeyPress
    , Del "interactions1"
    ]


schemaSlide : List Event
schemaSlide =
    [ AddSprite "schemaSchema" "schema" { x = 2, y = 12 }
    , KeyPress
    , Del "schemaSchema"
    ]


bugSlide : List Event
bugSlide =
    [ AddSprite "bugBug" "bug" { x = 0, y = 0 }
    , KeyPress
    , Del "bugBug"
    ]


immutabilitySlide : List Event
immutabilitySlide =
    [ AddText "immutability1" "\"Immutability" { x = 5, y = 29 }
    , AddText "immutability2" "is not enough\"" { x = 9, y = 40 }
    , AddText "immutability3" "Patrick Dubroy" { x = 5, y = 51 }
    , AddSprite "immutabilityCodewords" "codewords" { x = 25, y = 6 }
    , KeyPress
    , Del "immutability1"
    , Del "immutability2"
    , Del "immutability3"
    , Del "immutabilityCodewords"
    ]


renderingSlide : List Event
renderingSlide =
    [ AddText "rendering1" "3. Rendering" { x = 10, y = 21 }
    , KeyPress
    , Del "rendering1"
    ]


textureSlide : List Event
textureSlide =
    [ AddSprite "textureTexture" "texture" { x = 0, y = 0 }
    , KeyPress
    , AddSprite "textureSelection" "selection" { x = 0, y = 15 }
    , KeyPress
    , Del "textureSelection"
    , Move "textureTexture" { x = -100, y = 0, duration = 2750 }
    , KeyPress
    , Del "textureTexture"
    ]


wisdomSlide : List Event
wisdomSlide =
    [ AddSprite "recapElmBig" "elm-big" { x = 24, y = 5 }
    , AddText "recap1" "Slack:" { x = 23, y = 27 }
    , AddText "recap2" "#gamedev\n #webgl" { x = 16, y = 38 }
    ]


initial : Engine
initial =
    Engine.initial <|
        List.concat
            [ titleSlide
            , aboutSlide
            , carmackSlide
            , whyGamesInElmSlide
            , myGamesInElmSlide
            , challengesSlide
            , gameStateSlide
            , marioModelSlide
            , elmStreet404ModelSlide
            , mogeeModelSlide
            , ecsSlide
            , interactionsSlide
            , schemaSlide
            , bugSlide
            , immutabilitySlide
            , renderingSlide
            , textureSlide
            , wisdomSlide
            ]
